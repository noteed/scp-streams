{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as B
import qualified System.IO.Streams as S

import Network.SCP.Protocol

main :: IO ()
main = do
  scp <- start
  _ <- copy scp 0 7 5 5 1 "a"
  _ <- S.fromByteString "A" >>= send scp
  _ <- stop scp
  "A" <- B.readFile "/tmp/a"
  return ()
