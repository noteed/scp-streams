{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SCP.Protocol where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Word (Word8)
import System.Exit (ExitCode(..))
import System.IO (hClose, hFlush, hPutStrLn, stderr)
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

sendSsh :: String -> String -> Maybe FilePath -> FilePath -> IO SCP
sendSsh user host midentity target = sendProcess "ssh" $
  [ "-q"
  -- TODO actually use known_hosts and key check.
  , "-o", "UserKnownHostsFile=/dev/null"
  , "-o", "StrictHostKeyChecking=no"
  ] ++
  maybe [] (\i -> ["-i", i]) midentity ++
  [ user ++ "@" ++ host
  , "scp"
  , "-r" -- TODO Only when pushing directories.
         -- TODO Error out when receiving directories without -r.
  , "-t"
  , target
  ]

sendStd :: IO SCP
sendStd = do
  let scp = SCP S.stdout S.stdin Nothing
  _ <- startSending scp
  return scp

sendSelf :: FilePath -> IO SCP
sendSelf target = sendProcess "dist/build/scp-streams/scp-streams" ["-t", target]

sendDirect :: FilePath -> IO SCP
sendDirect target = sendProcess "scp" ["-t", target]

stop :: SCP -> IO ExitCode
stop scp@SCP{..} = do
  stopSending scp
  maybe (return ExitSuccess) P.waitForProcess scpProcess

copy :: SCP -> Word8 -> Word8 -> Word8 -> Word8 -> Int -> ByteString
  -> InputStream ByteString -> IO Bool
copy scp a b c d len filename content = do
  _ <- sendCommand scp $ Copy a b c d len filename
  sendContent scp content

push :: SCP -> Word8 -> Word8 -> Word8 -> Word8 -> ByteString -> IO Bool
push scp a b c d directory = sendCommand scp $ Push a b c d directory

pop :: SCP -> IO Bool
pop scp = sendCommand scp Pop

receiveSsh :: String -> String -> String -> IO SCP
receiveSsh user host path = receiveProcess "ssh"
  [ "-q"
  -- TODO actually use known_hosts and key check.
  , "-o", "UserKnownHostsFile=/dev/null"
  , "-o", "StrictHostKeyChecking=no"
  , "-i", "/home/scp/.ssh/insecure_id_rsa"
  , user ++ "@" ++ host
  , "scp"
  , "-r" -- TODO Only when pulling directories.
  , "-f"
  , path
  ]

receiveStd :: IO SCP
receiveStd = do
  let scp = SCP S.stdout S.stdin Nothing
  startReceiving scp
  return scp

receiveDirect :: String -> IO SCP
receiveDirect path = receiveProcess "scp" ["-f", path]

readCommand :: SCP -> IO Command
readCommand SCP{..} = do
  command <- S.parseFromStream commandParser scpOut
  confirm scpIn
  return command

contentAsInputStream :: SCP -> Int -> IO (InputStream ByteString)
contentAsInputStream SCP{..} len = do
  is <- S.takeBytes (fromIntegral len) scpOut
  is' <- S.atEndOfInput (do
    _ <- getFeedback scpOut
    confirm scpIn) is
  return is'

doneReceiving :: SCP -> IO Bool
doneReceiving SCP{..} = S.atEOF scpOut

----------------------------------------------------------------------
-- "Low-level" interface
----------------------------------------------------------------------

sendProcess :: FilePath -> [String] -> IO SCP
sendProcess cmd args = do
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
#ifdef SCP_DEBUG
  hPutStrLn stderr ("Sending command " ++ C.unpack c) >> hFlush stderr
#endif
  S.writeLazyByteString (L.fromChunks [c, "\n"]) scpIn
  S.write (Just "") scpIn -- flush
  getFeedback scpOut

sendContent :: SCP -> InputStream ByteString -> IO Bool
sendContent SCP{..} content = do
#ifdef SCP_DEBUG
  hPutStrLn stderr "Sending content..." >> hFlush stderr
#endif
  S.supply content scpIn
  confirm scpIn
  b <- getFeedback scpOut
  return b

stopSending :: SCP -> IO ()
stopSending SCP{..} = S.write Nothing scpIn

receiveProcess :: String -> [String] -> IO SCP
receiveProcess cmd args = do
  (inp, out, h) <- runInteractiveProcess cmd args
  let scp = SCP inp out $ Just h
  startReceiving scp
  return scp

startReceiving :: SCP -> IO ()
startReceiving SCP{..} = do
  confirm scpIn

getFeedback :: InputStream ByteString -> IO Bool
getFeedback feedback = do
  i' <- S.readExactly 1 feedback
  case i' of
    "\0" -> return True
    _ -> do
      msg <- sGetLine feedback
#ifdef SCP_DEBUG
      hPutStrLn stderr ("Bad feedback: " ++ C.unpack msg) >> hFlush stderr
#endif
      return False

confirm :: OutputStream ByteString -> IO ()
confirm os = do
  S.writeLazyByteString "\0" os
  S.write (Just "") os -- flush

-- | Send an error message. The other end will exit.
whine :: OutputStream ByteString -> ByteString -> IO ()
whine os msg = do
  S.writeLazyByteString (L.fromChunks ["\x02", msg, "\n"]) os
  S.write (Just "") os -- flush

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
