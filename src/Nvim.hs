{-# LANGUAGE LambdaCase, DeriveGeneric, DuplicateRecordFields #-}
module Nvim where

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString as T
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as I
import System.Process
import System.IO
import Data.List


data Req = Req
  { registers :: [Register]
  , input     :: String
  , buffer    :: String
  } deriving (Generic, Show)

data Register = Register
  { name  :: Char
  , value :: String
  } deriving (Generic, Show)

instance ToJSON Req
instance ToJSON Register

test = Req
  { input     = ""
  , registers = [ Register 'a' "lorem\nipsum" ]
  , buffer    = "lorem\nipsum"
  }

keyEscape = 27

exec :: Req -> IO I.ByteString
exec r = do
  (Just inp, Just out, err, ph)
    <- createProcess (proc "python" ["nvim/pipe.py"])
                     { std_in  = CreatePipe
                     , std_out = CreatePipe }
  -- print r
  I.hPutStr inp . L.toStrict $ encode r
  hClose inp
  I.hGetContents out

