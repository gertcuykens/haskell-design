{-# LANGUAGE OverloadedStrings #-}
module Picture (fileServer) where

import Data.Monoid (mappend)
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as S
import qualified Data.Text.IO as S
import qualified Data.Text.Lazy.Internal as L (Text)
import qualified Data.Text.Lazy.IO as L
import qualified Data.ByteString.Char8 as C (pack)
import qualified Network.WebSockets as WS
import qualified Login as FB
import qualified Json as JS
import File

catchDisconnect :: SomeException -> WS.WebSockets WS.Hybi10 ()
catchDisconnect e =
    case fromException e of
        Just WS.ConnectionClosed -> liftIO $ L.putStrLn "Connection Closed"
        _ -> return ()

loop :: FB.UserId -> WS.WebSockets WS.Hybi10 ()
loop i = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ L.writeFile "test/test.txt" msg
    --liftIO $ L.putStrLn msg
    --case msg of
    --s <- liftIO $ JS.readS i state
    WS.sendTextData ("ok"::L.Text)
    --liftIO $ JS.writeS i msg state
    loop i

application :: WS.Request -> WS.WebSockets WS.Hybi10 ()
application rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    msg <- WS.receiveData
    liftIO $ S.putStrLn msg
    let prefix = "Facebook Code "
    let code = S.unpack $ S.drop (S.length prefix) msg
    i <- liftIO (try $ FB.uid  ((\(x,y) -> (C.pack x, C.pack y)) ("code", code)) :: IO (Either SomeException (FB.UserId)))
    case i of
        Right i -> loop i
        Left _ -> do url <- liftIO FB.url; WS.sendTextData ("Facebook Login " `mappend` url :: S.Text)

fileServer :: IO ()
fileServer = WS.runServer "0.0.0.0" 9162 $ application
