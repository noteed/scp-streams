{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Version (showVersion)
import Paths_scp_streams (version)
import System.Console.CmdArgs.Implicit

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
  deriving (Data, Typeable)

-- | Create the (default) 'Scp' command.
cmdScp :: Cmd
cmdScp = CmdScp
    &= help "Drop-in `scp` replacement."
    &= explicit
    &= name "scp"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd CmdScp{..} = do
  return ()
