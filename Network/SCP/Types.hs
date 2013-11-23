{-# LANGUAGE OverloadedStrings #-}
module Network.SCP.Types where

import Control.Applicative ((<$>), (<|>), (<$))
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Prelude hiding (takeWhile)
import Data.Word (Word8)

data Command =
    Copy Word8 Word8 Word8 Word8 Int ByteString
  -- ^ Copy a file. Permissions (4 bytes), file size in bytes, filename.
  | Push Word8 Word8 Word8 Word8 ByteString
  -- ^ Enter directory. Permissions, directory name.
  | Pop
  -- ^ Exit directory.
  deriving Show

-- e.g. C0755 567 run.sh
commandParser :: Parser Command
commandParser = copyParser <|> pushParser <|> popParser

copyParser :: Parser Command
copyParser = do
  _ <- char 'C'
  (a, b, c, d) <- permissionsParser
  _ <- char ' '
  size <- decimal
  _ <- char ' '
  filename <- takeWhile (/= '\n') -- TODO enforce correct filename (no slash, no single dot, ...)
  _ <- char '\n'
  return $ Copy a b c d size filename

pushParser :: Parser Command
pushParser = do
  _ <- char 'D'
  (a, b, c, d) <- permissionsParser
  _ <- string " 0 "
  dir <- takeWhile (/= '\n')
  _ <- char '\n'
  return $ Push a b c d dir

popParser :: Parser Command
popParser = Pop <$ string "E\n"

permissionsParser :: Parser (Word8, Word8, Word8, Word8)
permissionsParser = do
  a <- read . (:[]) <$> digit
  b <- read . (:[]) <$> digit
  c <- read . (:[]) <$> digit
  d <- read . (:[]) <$> digit
  return (a, b, c, d)

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
