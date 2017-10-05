{-# LANGUAGE LambdaCase, DeriveGeneric, DuplicateRecordFields #-}
module Halovi where

import System.Process
import System.IO
import Control.Monad
import Control.Monad.Loops
import Data.List
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as I
import Data.Char


data Op = Open Str' | Input Str' | Search Str' | Query Str'
        | Quit | QuitAll
        | Loop [Op] | Group [Op] | Repeat Int Op
        | Prev | Next | Click
        | NextPage | PrevPage | NextOfType
        | YankText Str | YankURL Str | YankAttribute Str Str'
        | GoUp | GoRoot | GoTop | GoBottom
        | NOP
        deriving Show

data Str  = Reg Char | Chr Char deriving Show
type Str' = [Str]
type Program  = [Op]

data PState = PState
  { registers :: M.Map Char String
  , hInput    :: Handle
  , hOutput   :: Handle
  , debug     :: Bool
  }

getReg name = fromMaybe "" . M.lookup name <$> gets registers

setReg r v = case r of
  Reg '"' -> liftIO $ putStrLn v
  Reg x   -> lift . modify $ \s -> s
    { registers = M.insert x v $ registers s }

data Response = Response
  { respCode :: RespCode
  , respMsg  :: String
  } deriving (Generic, Show)

data RespCode
  = READY | SUCCESS | FAILURE | LOG
  deriving (Generic, Show, Eq)

instance FromJSON Response
instance FromJSON RespCode


data Request = Request
  { reqCode :: ReqCode
  , reqMsg  :: String
  } deriving (Generic, Show)
data ReqCode
  = EXEC | LOREM
  deriving (Generic, Show, Eq)

instance ToJSON Request
instance ToJSON ReqCode


getResponse = do
  out <- lift $ gets hOutput
  deb <- lift $ gets debug
  ln  <- liftIO $ T.hGetLine out
  when deb . liftIO $ I.putStrLn ln

  case decode $ L.fromStrict ln of
    Just x -> return x
    _   -> do
      getResponse

sendRequest req = do
  inp <- lift $ gets hInput
  deb <- lift $ gets debug
  when deb . liftIO . I.putStrLn . L.toStrict $ encode req

  liftIO $ do
    I.hPutStrLn inp . L.toStrict $ encode req
    hFlush inp

msg req = do
  sendRequest req
  f
 where f = do
         resp <- getResponse
         case resp of
           Response FAILURE _ -> mzero
           Response LOG     _ -> f
           _                  -> return resp

-------------

run deb headful args p = do
  let nodeArgs = "nm/main.js"
               : [ "headful" | headful ]

  (Just inp, Just out, err, ph)
    <- createProcess (proc "node" nodeArgs)
                     { std_in  = CreatePipe
                     , std_out = CreatePipe }

  let st = PState
        { registers = M.fromList $ zip (show =<< [0..9]) args
        , hInput    = inp
        , hOutput   = out
        , debug     = deb
        }

  when deb $ print p

  void . flip runStateT st $ do
    void $ runMaybeT $ do
      whileM_ ((READY/=).respCode <$> getResponse) (return ())
      when deb . liftIO $ putStrLn "GOT READY"
      mapM_ runOp p
      runOp QuitAll


str str = do
  res <- forM str $ \case
    Chr x -> return [x]
    Reg x -> lift $ getReg x
  -- liftIO $ putStrLn $ concat res
  return $ concat res

formatURL url
  | "http://" `isPrefixOf` url = url
  | "."       `isInfixOf`  url = "http://" ++ url
  | otherwise = "https://www.google.com/search?pws=0&gl=us&gws_rd=cr&q=" ++ url

------------

runOp (Open url) = do
  url' <- str url
  void . msg $ Request EXEC $ "this.open(" ++ show (formatURL url') ++ ")"

runOp QuitAll = sendRequest $ Request EXEC "this.quitAll()"

runOp (Repeat n (Input text)) = do
  text' <- str text
  void . msg $ Request EXEC $ "this.input(" ++ show text' ++ ", " ++ show n ++ ")"
runOp (Input text) = do
  text' <- str text
  void . msg $ Request EXEC $ "this.input(" ++ show text' ++ ", 0)"

runOp (Search text) = do
  text' <- str text
  void . msg $ Request EXEC $ "this.search(" ++ show text' ++ ")"

runOp (Query text) = do
  text' <- str text
  void . msg $ Request EXEC $ "this.query(" ++ show text' ++ ")"

runOp (YankText r) = do
  Response SUCCESS answ <- msg $ Request EXEC "this.yankText()"
  setReg r answ
runOp (YankURL r) = do
  Response SUCCESS answ <- msg $ Request EXEC "this.yankURL()"
  setReg r answ
runOp (YankAttribute r v) = do
  text' <- str v
  Response SUCCESS answ <- msg . Request EXEC $ "this.yankAttribute(" ++ show text' ++ ")"
  setReg r answ


runOp (Group x) = void . lift . runMaybeT $ forM_ x runOp
runOp (Loop  x) = void . lift . runMaybeT . forever $ forM_ x runOp


runOp (Repeat n GoTop) = void . msg . Request EXEC $ "this.goTop(" ++ show n ++ ")"
runOp (Repeat n GoBottom) = void . msg . Request EXEC $ "this.goBottom(" ++ show n ++ ")"

runOp (Repeat n (Loop x))
  = void . lift . runMaybeT . forM_ [1..n] . const $ forM_ x runOp
runOp (Repeat n o) = void . forM_ [1..n] . const $ runOp o


runOp x | (h:r) <- show x
        = void . msg . Request EXEC $ "this." ++ toLower h : r ++ "()"

