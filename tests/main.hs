{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.SCP.Protocol

main :: IO ()
main = do
  scp <- start
  _ <- copy scp 0 7 5 5 1 "a"
  _ <- send scp "A"
  _ <- stop scp
  return ()
