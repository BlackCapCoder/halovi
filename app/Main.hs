module Main where

import Halovi
import Parse

import System.Console.GetOpt
import System.Environment
import Control.Monad
import System.IO
import System.Exit

versionString = "Version 0.0.1"
programName   = "Halovi" ++ " - " ++ versionString

data Options = Options
  { optCode  :: IO String
  , optInput :: String
  }

startOptions :: Options
startOptions = Options { optCode  = getContents
                       , optInput = ""
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
              , optInput = input } = opts

  code >>= run (words input) . parse
