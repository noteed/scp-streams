{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SCP.Protocol where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.Process (ProcessHandle)
import System.Process (CreateProcess(..), StdStream(..))
import qualified System.Process as P

import Network.SCP.Types

data SCP = SCP
  { scpIn :: OutputStream ByteString
  , scpOut :: InputStream ByteString
  , scpProcess :: Maybe ProcessHandle
  }

----------------------------------------------------------------------
-- "Mid-level" interface
----------------------------------------------------------------------

sendSsh :: FilePath -> IO SCP
sendSsh target = error "TODO"

sendSelf :: FilePath -> IO SCP
sendSelf target = startProcess "dist/build/scp-streams/scp-streams" ["-t", target]

sendDirect :: FilePath -> IO SCP
sendDirect target = startProcess "scp" ["-t", target]

stop :: SCP -> IO ExitCode
stop scp@SCP{..} = do
  stopSending scp
  maybe (return ExitSuccess) P.waitForProcess scpProcess

copy :: SCP -> Word8 -> Word8 -> Word8 -> Word8 -> Int -> ByteString
  -> InputStream ByteString -> IO Bool
copy scp a b c d len filename content = do
  _ <- sendCommand scp $ Copy a b c d len filename
  sendContent scp content

receiveDirect = do
  confirm S.stdout
  S.write (Just "") S.stdout
  return $ SCP S.stdout S.stdin Nothing

readCommand :: SCP -> IO Command
readCommand SCP{..} = do
  command <- S.parseFromStream commandParser scpOut
  confirm scpIn
  S.write (Just "") scpIn
  return command

contentAsInputStream :: SCP -> Int -> IO (InputStream ByteString)
contentAsInputStream SCP{..} len = do
  is <- S.takeBytes (fromIntegral len) scpOut
  is' <- S.atEndOfInput (do
    _ <- getFeedback scpOut
    confirm scpIn
    S.write (Just "") scpIn) is
  return is'

doneReceiving :: SCP -> IO Bool
doneReceiving SCP{..} = S.atEOF scpOut

----------------------------------------------------------------------
-- "Low-level" interface
----------------------------------------------------------------------

startProcess :: FilePath -> [String] -> IO SCP
startProcess cmd args = do
  (inp, out, h) <- runInteractiveProcess cmd args
  let scp = SCP inp out $ Just h
  _ <- startSending scp
  return scp

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
  b <- getFeedback scpOut
  return b

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

whine :: OutputStream ByteString -> ByteString -> IO ()
whine os msg = S.writeLazyByteString (L.fromChunks ["\x01", msg, "\n"]) os

sGetLine :: InputStream ByteString -> IO ByteString
sGetLine is = do
  mline <- S.takeBytesWhile (/= '\n') is
  return $ maybe "" B.init mline

-- | Similar to S.runInteractiveProcess but without the stderr pipe.
runInteractiveProcess :: String -> [String]
  -> IO (OutputStream ByteString, InputStream ByteString, ProcessHandle)
runInteractiveProcess cmd args = do
  (Just hin, Just hout , _, ph) <- P.createProcess (P.proc cmd args)
    { std_in = CreatePipe, std_out = CreatePipe }
  sIn  <- S.handleToOutputStream hin >>=
          S.atEndOfOutput (hClose hin) >>=
          S.lockingOutputStream
  sOut <- S.handleToInputStream hout >>=
          S.atEndOfInput (hClose hout) >>=
          S.lockingInputStream
  return (sIn, sOut, ph)
