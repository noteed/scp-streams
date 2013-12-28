-- Simulate a network failure or a Ctrl-C on the client when a file is being
-- uploaded. Run against a regular remote `scp`, the file is created but empty.
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified System.IO.Streams as S
import System.Environment (getArgs)

import Network.SCP.Protocol
import Network.SCP.Types (Command(Copy))

main :: IO ()
main = do
  [user, host, target] <- getArgs
  scp <- sendSsh user host target
  -- A 10-byte size is advertized...
  _ <- sendCommand scp $ Copy 0 7 5 5 10 "a"
  -- ... but 4-byte input stream is given
  content <- S.fromByteString "AAAA"
  S.connect content (scpIn scp)
