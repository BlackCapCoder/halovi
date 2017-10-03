{-# LANGUAGE LambdaCase #-}
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

data Op = Open Str' | Input Str' | Search Str' | Query Str'
        | Quit | QuitAll
        | Loop [Op] | Group [Op] | Repeat Int Op
        | Yank | Prev | Next | Click
        | NextPage | PrevPage | NextOfType
        deriving Show

data Str  = Reg Char | Chr Char deriving Show
type Str' = [Str]
type Program  = [Op]

data PState = PState
  { registers :: M.Map Char String
  }

run args p = do
  let st = PState
        { registers = M.fromList $ zip (show =<< [0..9]) args
        }

  (Just inp, Just out, err, pid)
    <- createProcess (proc "node" ["nm/main.js"])
                     { std_in  = CreatePipe
                     , std_out = CreatePipe }
  whileM_ ((/="ready") <$> hGetLine out) (return ())
  void . flip runStateT st $ mapM_ (\x -> liftIO (print x) >> runOp inp out x) p

getReg name = fromMaybe "" . M.lookup name <$> gets registers

str str = do
  res <- forM str $ \case
    Chr x -> return [x]
    Reg x -> getReg x
  liftIO $ putStrLn $ concat res
  return $ concat res


runOp inp out (Open url) = do
  url' <- str url
  answ <- message inp out $ "this.open(\"" ++ formatURL url' ++ "\")"
  handleErr answ

runOp inp out (Input text) = do
  text' <- str text
  answ <- message inp out $ "this.input(\"" ++ text' ++ "\")"
  handleErr answ

runOp inp out (Search text) = do
  text' <- str text
  answ <- message inp out $ "this.search(\"" ++ text' ++ "\")"
  handleErr answ

runOp inp out (Query text) = do
  text' <- str text
  answ <- message inp out $ "this.query(\"" ++ text' ++ "\")"
  handleErr answ

runOp inp out Yank = do
  answ <- message inp out "this.yank()"
  liftIO $ putStrLn answ
  return $ Just ()

runOp inp out Next = do
  answ <- message inp out "this.next()"
  handleErr answ

runOp inp out Click = do
  answ <- message inp out "this.click()"
  handleErr answ

runOp inp out NextPage = do
  answ <- message inp out "this.nextPage()"
  handleErr answ

runOp inp out PrevPage = do
  answ <- message inp out "this.prevPage()"
  handleErr answ

runOp inp out NextOfType = do
  answ <- message inp out "this.nextOfType()"
  handleErr answ

runOp inp out (Loop x) = do
  _ <- runMaybeT . forever $
    forM_ x $ \a -> do
      q <- lift $ runOp inp out a
      when (q==Nothing) mzero
  return $ Just ()

runOp inp out (Repeat n (Loop x)) = do
  _ <- runMaybeT . forM_ [1..n] . const $
    forM_ x $ \a -> do
      q <- lift $ runOp inp out a
      when (q==Nothing) mzero
  return $ Just ()

runOp inp out (Group x) =
  runMaybeT $
    forM_ x $ \a -> do
      q <- lift $ runOp inp out a
      when (q==Nothing) mzero

runOp inp out (Repeat n o) = do
  forM_ [1..n] . const $ runOp inp out o
  return $ Just ()


runOp inp out Quit = message inp out "this.quit()" >> return (Just ())
runOp inp out QuitAll = message inp out "this.quitAll()" >> return Nothing


formatURL url
  | "http://" `isPrefixOf` url = url
  | "."       `isInfixOf`  url = "http://" ++ url
  | otherwise = "https://www.google.com/search?pws=0&gl=us&gws_rd=cr&q=" ++ url

message inp out msg = liftIO $ do
  hPutStrLn inp msg
  hFlush inp
  hGetLine out

handleErr code
  | code == "OK" = return $ Just ()
  | otherwise = return Nothing
