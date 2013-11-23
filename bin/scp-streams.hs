{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, when)
import qualified Data.ByteString.Char8 as C
import Data.Version (showVersion)
import Paths_scp_streams (version)
import System.Console.CmdArgs.Implicit
import System.IO (hFlush, hPutStrLn, stderr)
import qualified System.IO.Streams as S
import System.Posix.Files (fileSize, getFileStatus)

import Data.Digest.Pure.SHA
import System.IO.Streams.SHA

import Network.SCP.Protocol
import Network.SCP.Types

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdScp
    ]
  &= summary versionString
  &= program "scp-streams"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "scp-streams " ++ showVersion version ++ " - Copyright (c) 2013 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    CmdScp
  { cmdScpPaths :: [String]
  , cmdScpDirect :: Bool
  , cmdScpSelf :: Bool
  , cmdScpReceive :: Bool
  , cmdScpSend :: Bool
  }
  deriving (Data, Typeable)

-- | Create the (default) 'Scp' command.
cmdScp :: Cmd
cmdScp = CmdScp
  { cmdScpPaths = def
    &= args

  , cmdScpDirect = def
    &= help "Use the local `scp` instead of a remote `scp` through `ssh`."
    &= explicit
    &= name "direct"
  , cmdScpSelf = def
    &= help "Use the local `scp-streams` instead of a remote `scp` through `ssh`."
    &= explicit
    &= name "self"

  , cmdScpReceive = def
    &= help "Invoke as a sink."
    &= explicit
    &= name "t"
  , cmdScpSend = def
    &= help "Invoke as a source."
    &= explicit
    &= name "f"
  }
    &= help "Drop-in `scp` replacement."
    &= explicit
    &= name "scp"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd CmdScp{..} = do
  case (cmdScpReceive, cmdScpSend) of
    (True, True) -> error "-t and -f are exclusive."

    (True, False) -> receiveStd >>= receiveLoop

    (False, True) -> whine S.stdout "Source not implemented."

    (False, False) -> do
      when (length cmdScpPaths < 2) $
        error "Two or more filenames must be provided."
        -- If there is more than 2 filenames, the last one must be a directory.

      let s = if cmdScpDirect then sendDirect else sendSsh "TODO" "TODO"
          send = if cmdScpSelf then sendSelf else s

      -- Operate as a client.
      scp <- send $ last cmdScpPaths
      forM_ (init cmdScpPaths) $ \p -> do
        size <- fileSize <$> getFileStatus p
        S.withFileAsInput p $ \is -> do
          (is1, getSha1) <- sha1Input is
          _ <- copy scp 0 7 5 5 (fromIntegral size) (C.pack p) is1
          getSha1 >>= putStrLn . ("SHA1: " ++ ) . showDigest
          return ()
      _ <- stop scp
      return ()

receiveLoop :: SCP -> IO ()
receiveLoop scp = do
  done <- doneReceiving scp
  if done
    then return ()
    else do
      command@(Copy a b c d len filename) <- readCommand scp
      is <- contentAsInputStream scp len
      (is1, getSha1) <- sha1Input is
      S.skipToEof is1
      getSha1 >>= hPutStrLn stderr . ("SHA1: " ++ ) . showDigest
      hFlush stderr
      receiveLoop scp
