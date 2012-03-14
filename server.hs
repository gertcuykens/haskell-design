-- Copyright(c) gert.cuykens@gmail.com
import Network
import System.IO
import Control.Concurrent
import Control.Monad(when)
import Char

serverHandshake :: String
serverHandshake = 
 "HTTP/1.1 101 Switching Protocols\r\n\
 \Upgrade: WebSocket\r\n\
 \Connection: Upgrade\r\n\
 \Sec-WebSocket-Accept: *******************\r\n\r\n"

acceptLoop socket = forever $ do
 (h,_,_) <- accept socket
 ------------------------
 hPutStr h serverHandshake
 hSetBuffering h NoBuffering
 forkIO (listenLoop h)  
 where
  forever a = do a; forever a

main = withSocketsDo $ do
 h <- listenOn (PortNumber 8000)
 acceptLoop h
 sClose h
 return ()

listenLoop :: Handle  -> IO ()
listenLoop h = do
 sendFrame h "aleloia"
 msg <- readFrame h
 putStrLn msg
 when (msg /= "quit") (listenLoop h)

readFrame :: Handle -> IO String
readFrame h = readUntil h ""
 where
  readUntil h str = do
   new <- hGetChar h
   if new == chr 0
    then readUntil h ""
    else if new == chr 255
     then return str
     else readUntil h (str ++ [new])

sendFrame :: Handle -> String -> IO ()
sendFrame h s = do
 hPutChar h (chr 0)
 hPutStr h s
 hPutChar h (chr 255)

