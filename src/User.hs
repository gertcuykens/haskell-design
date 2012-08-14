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

loop :: ST.State -> WS.WebSockets WS.Hybi10 ()
loop state = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ T.putStrLn msg
    liftIO $ ST.writeS [msg] state
    s <- liftIO $ ST.readS state
    WS.sendTextData (head s)
    loop state
    where
        catchDisconnect e = case fromException e of
            Just WS.ConnectionClosed -> liftIO $ T.putStrLn "Connection Closed"
            _ -> return ()

application :: ST.State -> WS.Request -> WS.WebSockets WS.Hybi10 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    msg <- WS.receiveData
    liftIO $ T.putStrLn msg
    let prefix = "Facebook Code "
    let code = T.unpack $ T.drop (T.length prefix) msg
    e <- liftIO (try $ FB.fbName  ((\(x,y) -> (C.pack x, C.pack y)) ("code", code)) :: IO (Either SomeException (Maybe Text)))
    case e of
        Left _ -> do url <- liftIO FB.fbUrl; WS.sendTextData ("Facebook Login " `mappend` url :: Text)
        Right Nothing -> liftIO $ T.putStrLn "Facebook Error"
        Right (Just e) -> loop state

userServer :: IO ()
userServer = do
    s <- ST.newS
    WS.runServer "0.0.0.0" 9161 $ application s
