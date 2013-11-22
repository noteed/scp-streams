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
import qualified System.IO.Streams as S
import System.Posix.Files (fileSize, getFileStatus)

import Data.Digest.Pure.SHA
import System.IO.Streams.SHA

import Network.SCP.Protocol

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
  }
  deriving (Data, Typeable)

-- | Create the (default) 'Scp' command.
cmdScp :: Cmd
cmdScp = CmdScp
  { cmdScpPaths = def
    &= args
  }
    &= help "Drop-in `scp` replacement."
    &= explicit
    &= name "scp"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd CmdScp{..} = do
  when (length cmdScpPaths < 2) $
    error "Two or more filenames must be provided."
    -- If there is more than 2 filenames, the last one must be a directory.
  scp <- start $ last cmdScpPaths
  forM_ (init cmdScpPaths) $ \p -> do
    size <- fileSize <$> getFileStatus p
    _ <- copy scp 0 7 5 5 (fromIntegral size) $ C.pack p
    S.withFileAsInput p $ \is -> do
      (is1, getSha1) <- sha1Input is
      _ <- send scp is1
      getSha1 >>= putStrLn . ("SHA1: " ++ ) . showDigest
      return ()
  _ <- stop scp
  return ()
