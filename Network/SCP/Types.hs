{-# LANGUAGE OverloadedStrings #-}
module Network.SCP.Types where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)

data Command =
    Copy Word8 Word8 Word8 Word8 Int ByteString
  -- ^ Copy a file. Permissions (4 bytes), file size in bytes, filename.
  | Push Word8 Word8 Word8 Word8 ByteString
  -- ^ Enter directory. Permissions, directory name.
  | Pop
  -- ^ Exit directory.
  deriving Show

-- TODO use cereal
unparse :: Command -> ByteString
unparse command = case command of
  (Copy a b c d size filename) ->
    C.pack ['C', head $ show a, head $ show b, head $ show c, head $ show d]
      `B.append` " " `B.append` C.pack (show size) `B.append` " "
      `B.append` filename
  (Push a b c d filename) ->
    C.pack ['D', head $ show a, head $ show b, head $ show c, head $ show d]
      `B.append` " 0 " `B.append` filename
  Pop -> "E"
