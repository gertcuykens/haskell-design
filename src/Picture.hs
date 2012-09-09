{-# LANGUAGE OverloadedStrings #-}
module Picture (picture) where

import Data.Monoid (mappend)
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as C (pack,unpack)
import qualified Network.WebSockets as WS
import qualified Login as LG

mkdir :: FilePath -> IO ()
mkdir = createDirectoryIfMissing False

save :: FilePath -> B.ByteString -> IO ()
save = B.writeFile

load :: FilePath -> IO B.ByteString
load = B.readFile

catchDisconnect :: SomeException -> WS.WebSockets WS.Hybi10 ()
catchDisconnect e =
    case fromException e of
        Just WS.ConnectionClosed -> liftIO $ B.putStrLn "Connection Closed"
        _ -> return ()

loop :: String -> WS.WebSockets WS.Hybi10 ()
loop p = flip WS.catchWsError catchDisconnect $ do
    f <- liftIO $ load (p ++ "/picture.png")
    WS.sendBinaryData (f)
    m <- WS.receiveData
    liftIO $ save (p ++ "/picture.png") m
    loop p

picture :: WS.Request -> WS.WebSockets WS.Hybi10 ()
picture rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    msg <- WS.receiveData
    liftIO $ B.putStrLn msg
    let prefix = "Facebook Code "
    let code = B.unpack $ B.drop (B.length prefix) msg
    i <- liftIO (try $ LG.uid (C.pack "code",C.pack code) :: IO (Either SomeException String))
    case i of
        Right i -> do
            let p = "data/" ++ i
            liftIO $ mkdir p
            WS.sendTextData (B.pack("Facebook Uid " ++ i))
            loop p
        Left _ -> do
            url <- liftIO LG.url
            WS.sendTextData ("Facebook Login " `mappend` url)
