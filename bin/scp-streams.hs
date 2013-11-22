{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Version (showVersion)
import Paths_scp_streams (version)
import System.Console.CmdArgs.Implicit
import System.Exit (ExitCode)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import System.Process (ProcessHandle)
import qualified System.Process as P

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
  (inp, out, err, h) <- S.runInteractiveProcess "scp" ["-t", "/tmp/a"] Nothing Nothing
  let scp = SCP inp out err h
  _ <- startSending scp
  _ <- sendCommand scp $ Copy 0 7 5 5 1 "a"
  _ <- sendContent scp "A"
  _ <- stopSending scp
  return ()

data SCP = SCP
  { scpIn :: OutputStream ByteString
  , scpOut :: InputStream ByteString
  , scpErr :: InputStream ByteString
  , scpProcess :: ProcessHandle
  }

startSending :: SCP -> IO Bool
startSending SCP{..} = do
  getFeedback scpOut

sendCommand :: SCP -> Command -> IO Bool
sendCommand SCP{..} command = do
  let c = unparse command
  putStrLn $ "Sending command " ++ C.unpack c
  S.writeLazyByteString (L.fromChunks [c, "\n"]) scpIn
  S.write (Just "") scpIn
  getFeedback scpOut

sendContent :: SCP -> ByteString -> IO Bool
sendContent SCP{..} content = do
  putStrLn "Sending content..."
  S.writeLazyByteString (L.fromChunks [content]) scpIn
  confirm scpIn
  S.write (Just "") scpIn
  getFeedback scpOut

stopSending :: SCP -> IO ExitCode
stopSending SCP{..} = do
  S.write Nothing scpIn
  P.waitForProcess scpProcess

getFeedback :: InputStream ByteString -> IO Bool
getFeedback feedback = do
  i' <- S.readExactly 1 feedback
  case i' of
    "\0" -> return True
    _ -> do
      msg <- sGetLine feedback
      putStrLn $ "Bad feedback: " ++ C.unpack msg
      return False

confirm :: OutputStream ByteString -> IO ()
confirm os = S.writeLazyByteString "\0" os

sGetLine :: InputStream ByteString -> IO ByteString
sGetLine is = do
  mline <- S.takeBytesWhile (/= '\n') is
  return $ maybe "" B.init mline
