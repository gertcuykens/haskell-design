{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Chat (chatServer) where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text, pack)
import Control.Exception
import Control.Monad (forM_)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import qualified Data.ByteString.Char8 as C
import qualified Login as FB

type Counter = Int
type Client = (Text, WS.Sink WS.Hybi10)
type ServerState = (Counter,[Client])

inc :: Counter -> Counter
inc i = i + 1

counter :: ServerState -> Counter
counter (a,_) = a

clients :: ServerState -> [Client]
clients (_,b) = b

clientExists :: Client -> [Client] -> Bool
clientExists c = any ((== fst c) . fst)

addClient :: Client -> [Client] -> [Client]
addClient c cs = c : cs

removeClient :: Client -> [Client] -> [Client]
removeClient c = filter ((/= fst c) . fst)

broadcast :: Text -> [Client] -> IO ()
broadcast msg cs = do
    T.putStrLn msg
    forM_ cs $ \(_, sink) -> WS.sendSink sink $ WS.textData msg

talk :: MVar ServerState -> Client -> WS.WebSockets WS.Hybi10 ()
talk state c@(u, _) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    s <- liftIO $ readMVar state
    let i = inc (counter s)
    let cs = clients s
    liftIO $ broadcast (pack (show i) `mappend` " " `mappend` u `mappend` ": " `mappend` msg) cs
    liftIO $ modifyMVar_ state $ \s -> return (i,cs)
    talk state c
    where
        catchDisconnect e = case fromException e of
            Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
                let i = counter s
                let cs = clients s
                let s' = removeClient c cs
                broadcast (u `mappend` " disconnected") s'
                return (i,s')
            _ -> return ()

application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
application state rq = do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)
    msg <- WS.receiveData
    let prefix = "Facebook Code "
    let code = T.unpack $ T.drop (T.length prefix) msg
    e <- liftIO (try $ FB.fbName  ((\(x,y) -> (C.pack x, C.pack y)) ("code", code)) :: IO (Either SomeException (Maybe Text)))
    case e of
        Left _ -> do url <- liftIO FB.fbUrl; WS.sendTextData ("Facebook Login " `mappend` url :: Text)
        --Left e -> liftIO $ print $ "error: " ++ show (e :: SomeException)
        Right Nothing -> return ()
        Right (Just e) -> do
            s <- liftIO $ readMVar state
            sink <- WS.getSink
            let i = counter s
            let c = (e, sink)
            let cs = clients s
            if clientExists c cs
                then WS.sendTextData ("User already exists" :: Text)
                else do
                    liftIO $ modifyMVar_ state $ \s -> do
                        let s' = addClient c cs
                        WS.sendSink sink $ WS.textData $ "Facebook Users " `mappend` T.intercalate ", " (map fst cs)
                        broadcast (fst c `mappend` " joined") s'
                        return (i,s')
                    talk state c

chatServer :: IO ()
chatServer = do
    state <- newMVar (0,[])
    WS.runServer "0.0.0.0" 9160 $ application state
