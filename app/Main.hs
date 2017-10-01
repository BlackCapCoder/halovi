module Main where

import Halovi
import Parse

import System.Console.GetOpt
import System.Environment
import Control.Monad
import System.IO
import System.Exit

versionString = "Version 0.1"
programName   = "Halovi" ++ " - " ++ versionString

data Options = Options
  { optCode  :: IO String
  , optInput :: String
  , optDeb   :: Bool
  , optHead  :: Bool
  }

startOptions :: Options
startOptions = Options { optCode  = getContents
                       , optInput = ""
                       , optDeb   = False
                       , optHead  = False
                       }

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "f" ["file"]
    (ReqArg
      (\arg opt -> return opt { optCode = readFile arg })
      "FILE")
    "File with code"
  , Option "c" ["code"]
    (ReqArg
      (\arg opt -> return opt { optCode = return arg })
      "CODE")
    "Program code"
  , Option "i" ["input"]
    (ReqArg
      (\arg opt -> return opt { optInput = arg })
      "STRING")
    "Program input"
  , Option "d" ["debug"]
    (NoArg
      (\opt -> return opt { optDeb = True }))
    "Enable debugging"
  , Option "H" ["headful"]
    (NoArg
      (\opt -> return opt { optHead = True }))
    "Enable headful"
  , Option "v" ["version"]
      (NoArg
          (\_ -> do
              hPutStrLn stderr versionString
              exitSuccess))
      "Print version"
  , Option "h" ["help"]
      (NoArg
          (\_ -> do
              hPutStrLn stderr (usageInfo programName options)
              exitSuccess))
      "Show help"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args

  opts <- foldl (>>=) (return startOptions) actions
  let Options { optCode  = code
              , optInput = input
              , optDeb   = debug
              , optHead  = headl } = opts

  code >>= run debug headl (words input) . parse
