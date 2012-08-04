{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Server (
    chatServer
) where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception
import Control.Monad (forM_)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as C
import Login

type Client = (Text, WS.Sink WS.Hybi10)
type ServerState = [Client]

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message

talk :: WS.Protocol p => MVar ServerState -> Client -> WS.WebSockets p ()
talk state client@(user, _) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    liftIO $ readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)
    talk state client
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = removeClient client s
            broadcast (user `mappend` " disconnected") s'
            return s'
        _ -> return ()

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    sink <- WS.getSink
    msg <- WS.receiveData
    clients <- liftIO $ readMVar state
    url <- liftIO fbUrl
    let prefix = "Facebook Code "
    let code = T.unpack $ T.drop (T.length prefix) msg
    e <- liftIO ( try $ fbName  ((\(x,y) -> (C.pack x, C.pack y)) ("code", code)
                ) :: IO (Either SomeException (Maybe Text)))
    case e of
        Left _ -> WS.sendTextData ("Facebook Login " `mappend` url :: Text)
        --Left e -> liftIO $ print $ "error: " ++ show (e :: SomeException)
        Right Nothing -> return ()
        Right (Just e) -> do
            let client = (e, sink)
            if clientExists client clients
                then WS.sendTextData ("User already exists" :: Text)
                else do
                    liftIO $ modifyMVar_ state $ \s -> do
                        let s' = addClient client s
                        WS.sendSink sink $ WS.textData $
                            "Facebook Users " `mappend`
                            T.intercalate ", " (map fst s)
                        broadcast (fst client `mappend` " joined") s'
                        return s'
                    talk state client

chatServer :: IO ()
chatServer = do
    state <- newMVar []
    WS.runServer "0.0.0.0" 9160 $ application state
