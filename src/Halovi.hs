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


data Op = Open Str' | Input Str' | Search Str' | Query Str'
        | Quit | QuitAll
        | Loop [Op] | Group [Op] | Repeat Int Op
        | Yank | Prev | Next | Click
        | NextPage | PrevPage | NextOfType
        | WindowSelection
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
  liftIO $ do
    I.hPutStrLn inp . L.toStrict $ encode req
    hFlush inp

msg req = do
  sendRequest req
  resp <- getResponse
  case resp of
    Response FAILURE _ -> mzero
    _                  -> return resp

-------------

run deb args p = do
  (Just inp, Just out, err, ph)
    <- createProcess (proc "node" ["nm/main.js"])
                     { std_in  = CreatePipe
                     , std_out = CreatePipe }

  let st = PState
        { registers = M.fromList $ zip (show =<< [0..9]) args
        , hInput    = inp
        , hOutput   = out
        , debug     = deb
        }

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
--

------------

runOp Prev            = void . msg $ Request EXEC "this.prev()"
runOp Next            = void . msg $ Request EXEC "this.next()"
runOp NextPage        = void . msg $ Request EXEC "this.nextPage()"
runOp PrevPage        = void . msg $ Request EXEC "this.prevPage()"
runOp NextOfType      = void . msg $ Request EXEC "this.nextOfType()"
runOp Click           = void . msg $ Request EXEC "this.click()"
runOp WindowSelection = void . msg $ Request EXEC "this.wopenSel()"
runOp Quit            = void . msg $ Request EXEC "this.quit()"

runOp (Open url) = do
  url' <- str url
  void . msg $ Request EXEC $ "this.open(\"" ++ formatURL url' ++ "\")"

runOp QuitAll = sendRequest $ Request EXEC "this.quitAll()"

runOp (Input text) = do
  text' <- str text
  void . msg $ Request EXEC $ "this.input(\"" ++ text' ++ "\")"

runOp (Search text) = do
  text' <- str text
  void . msg $ Request EXEC $ "this.search(\"" ++ text' ++ "\")"

runOp (Query text) = do
  text' <- str text
  void . msg $ Request EXEC $ "this.query(\"" ++ text' ++ "\")"

runOp Yank = do
  Response SUCCESS answ <- msg $ Request EXEC "this.yank()"
  liftIO $ putStrLn answ


runOp (Group x) = void . lift . runMaybeT $ forM_ x runOp
runOp (Loop  x) = void . lift . runMaybeT . forever $ forM_ x runOp

runOp (Repeat n (Loop x))
  = void . lift . runMaybeT . forM_ [1..n] . const $ forM_ x runOp
runOp (Repeat n o) = void . forM_ [1..n] . const $ runOp o

