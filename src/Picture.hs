{-# LANGUAGE OverloadedStrings #-}
module Picture (fileServer) where

--import System.Directory

import Data.Monoid (mappend)
import Control.Exception
import Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as C (pack)
import qualified Network.WebSockets as WS
import qualified Login as FB
import qualified Json as JS
import File

catchDisconnect :: SomeException -> WS.WebSockets WS.Hybi10 ()
catchDisconnect e =
    case fromException e of
        Just WS.ConnectionClosed -> liftIO $ B.putStrLn "Connection Closed"
        _ -> return ()

loop :: FB.UserId -> WS.WebSockets WS.Hybi10 ()
loop i = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ B.writeFile "test/test.txt" msg
    WS.sendBinaryData (msg)
    --liftIO $ B.putStrLn msg
    --case msg of
    loop i

application :: WS.Request -> WS.WebSockets WS.Hybi10 ()
application rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    msg <- WS.receiveData
    liftIO $ B.putStrLn msg
    let prefix = "Facebook Code "
    let code = B.unpack $ B.drop (B.length prefix) msg
    i <- liftIO (try $ FB.uid  ((\(x,y) -> (C.pack x, C.pack y)) ("code", code)) :: IO (Either SomeException (FB.UserId)))
    case i of
        Right i -> loop i
        Left _ -> do url <- liftIO FB.url; WS.sendBinaryData ("Facebook Login " `mappend` url)

fileServer :: IO ()
fileServer = WS.runServer "0.0.0.0" 9162 $ application
