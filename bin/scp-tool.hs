{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8 as BC
import qualified System.IO.Streams as S
import System.Environment (getArgs)

import Network.SCP.Protocol
import Network.SCP.Types (Command(Copy))

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["interrupted", user, host, target] ->
      sendInterrupted user host target
    "directories" : user : host : target : dirs ->
      sendDirectories user host Nothing target dirs
    "directories" : user : host : target : identity : dirs ->
      sendDirectories user host (Just identity) target dirs

-- | Simulate a network failure or a Ctrl-C on the client when a file is being
-- uploaded. Run against a regular remote `scp`, the file is created but empty.
sendInterrupted user host target = do
  scp <- sendSsh user host Nothing target
  -- A 10-byte size is advertized...
  _ <- sendCommand scp $ Copy 0 7 5 5 10 "a"
  -- ... but 4-byte input stream is given
  content <- S.fromByteString "AAAA"
  S.connect content (scpIn scp)

sendDirectories user host midentity target dirs = do
  scp <- sendSsh user host midentity target
  mapM_ (push scp 0 7 5 5 . BC.pack) dirs
  mapM_ (const $ pop scp) dirs
  _ <- stop scp
  return ()
