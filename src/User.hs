{-# LANGUAGE OverloadedStrings #-}
module User (user) where

import Data.Monoid (mappend)
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Acid
import qualified Data.Text as S
import qualified Data.Text.IO as S
import qualified Data.Text.Lazy.Internal as L (Text)
import qualified Data.Text.Lazy.IO as L
import qualified Data.ByteString.Char8 as C (pack)
import qualified Network.WebSockets as WS
import qualified Login as LG
import qualified Json as JS

catchDisconnect :: SomeException -> WS.WebSockets WS.Hybi10 ()
catchDisconnect e =
    case fromException e of
        Just WS.ConnectionClosed -> liftIO $ L.putStrLn "Connection Closed"
        _ -> return ()

loop :: AcidState JS.KeyValue -> String -> WS.WebSockets WS.Hybi10 ()
loop state k = flip WS.catchWsError catchDisconnect $ do
    s <- liftIO $ JS.readS state k
    WS.sendTextData (s)
    msg <- WS.receiveData
    liftIO $ L.putStrLn msg
    liftIO $ JS.writeS state k msg
    loop state k

user :: AcidState JS.KeyValue -> WS.Request -> WS.WebSockets WS.Hybi10 ()
user state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    msg <- WS.receiveData
    liftIO $ S.putStrLn msg
    let prefix = "Facebook Code "
    let code = S.unpack $ S.drop (S.length prefix) msg
    k <- liftIO (try $ LG.uid (C.pack "code",C.pack code) :: IO (Either SomeException String))
    case k of
        Right k -> do
            WS.sendTextData (C.pack("Facebook Uid " ++ k))
            loop state k
        Left _ -> do
            url <- liftIO LG.url
            WS.sendTextData ("Facebook Login " `mappend` url)
