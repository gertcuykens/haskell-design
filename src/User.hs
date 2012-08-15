{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module User (userServer) where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text, pack)
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as C
import qualified Login as FB
import qualified State as ST

catchDisconnect :: SomeException -> WS.WebSockets WS.Hybi10 ()
catchDisconnect e =
    case fromException e of
        Just WS.ConnectionClosed -> liftIO $ T.putStrLn "Connection Closed"
        _ -> return ()

loop :: FB.UserId -> ST.State -> WS.WebSockets WS.Hybi10 ()
loop i state = flip WS.catchWsError catchDisconnect $ do
    s <- liftIO $ ST.readS i state
    WS.sendTextData (s)
    msg <- WS.receiveData
    liftIO $ T.putStrLn msg
    liftIO $ ST.writeS i msg state
    loop i state

application :: ST.State -> WS.Request -> WS.WebSockets WS.Hybi10 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    msg <- WS.receiveData
    liftIO $ T.putStrLn msg
    let prefix = "Facebook Code "
    let code = T.unpack $ T.drop (T.length prefix) msg
    i <- liftIO (try $ FB.fbId  ((\(x,y) -> (C.pack x, C.pack y)) ("code", code)) :: IO (Either SomeException (FB.UserId)))
    case i of
        Right i -> loop i state
        Left _ -> do url <- liftIO FB.fbUrl; WS.sendTextData ("Facebook Login " `mappend` url :: Text)

userServer :: IO ()
userServer = do
    s <- ST.newS
    WS.runServer "0.0.0.0" 9161 $ application s
