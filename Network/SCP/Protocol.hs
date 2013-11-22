{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SCP.Protocol where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import System.Exit (ExitCode)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import System.Process (ProcessHandle)
import qualified System.Process as P

import Network.SCP.Types

data SCP = SCP
  { scpIn :: OutputStream ByteString
  , scpOut :: InputStream ByteString
  , scpErr :: InputStream ByteString
  , scpProcess :: ProcessHandle
  }

----------------------------------------------------------------------
-- "Mid-level" interface
----------------------------------------------------------------------

start :: FilePath -> IO SCP
start target = do
  (inp, out, err, h) <-
    S.runInteractiveProcess "scp" ["-t", target] Nothing Nothing
  let scp = SCP inp out err h
  _ <- startSending scp
  return scp

stop :: SCP -> IO ExitCode
stop scp@SCP{..} = do
  stopSending scp
  P.waitForProcess scpProcess

copy :: SCP -> Word8 -> Word8 -> Word8 -> Word8 -> Int -> ByteString -> IO Bool
copy scp a b c d len filename =
  sendCommand scp $ Copy a b c d len filename

send :: SCP -> InputStream ByteString -> IO Bool
send = sendContent

----------------------------------------------------------------------
-- "Low-level" interface
----------------------------------------------------------------------

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

sendContent :: SCP -> InputStream ByteString -> IO Bool
sendContent SCP{..} content = do
  putStrLn "Sending content..."
  S.supply content scpIn
  confirm scpIn
  S.write (Just "") scpIn
  getFeedback scpOut

stopSending :: SCP -> IO ()
stopSending SCP{..} = S.write Nothing scpIn

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
