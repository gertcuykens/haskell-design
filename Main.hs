{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (SomeException, try, fromException)
import Control.Lens (perform, traverse, act, _2)
--import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (newMVar, MVar, modifyMVar_, readMVar)
import System.Directory (createDirectoryIfMissing)
--import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Function.Pointless ((.:))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as BL
--import qualified Data.Text as T
--import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Lazy.Encoding as TL
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsPort)
import Network.Wai.Handler.WebSockets (intercept)
import qualified Network.WebSockets as WS
import qualified Json as JS
import qualified Login as FB

--import qualified Network.WebSockets.Monad as WS
--deriving instance Eq (WS.Sink WS.Hybi10)
--instance Eq (WS.Sink WS.Hybi10) where
--    WS.Sink a == WS.Sink b = a == b

type Counter = Int
type Client = (FB.User, WS.Sink WS.Hybi10)
type Clients = (Counter,[Client])

counter :: Clients -> Counter
counter (a,_) = a

clients :: Clients -> [Client]
clients (_,b) = b

clientExists :: Client -> [Client] -> Bool
clientExists c = any ((== snd c) . snd)

addClient :: Client -> [Client] -> [Client]
addClient c l = c : l

removeClient :: Client -> [Client] -> [Client]
removeClient c = filter ((/= snd c) . snd)

broadcast :: MonadIO m => T.Text -> [Client] -> m ()
broadcast t = liftIO .: perform $ traverse._2.act (`WS.sendSink` WS.textData t)
--broadcast t = (liftIO .) . perform $ traverse._2.act (`WS.sendSink` WS.textData t)
--broadcast t l = liftIO $ T.putStrLn t >> l ^! traverse . _2 . act (`WS.sendSink` WS.textData t)
--broadcast t l = liftIO (T.putStrLn t) >> liftIO (forM_ l $ \(_, k) -> WS.sendSink k $ WS.textData t)

loop1 :: MVar Clients -> Client -> WS.WebSockets WS.Hybi10 ()
loop1 s' c@(u,_) = flip WS.catchWsError catchDisconnect $ do
    m <- WS.receiveData
    s <- liftIO $ readMVar s'
    let i = counter s + 1
    let l = clients s
    let t = T.pack(show i) `mappend` " " `mappend` FB.name u `mappend` ": " `mappend` m
    liftIO $ T.putStrLn t
    broadcast t l
    liftIO $ modifyMVar_ s' $ \_ -> return (i,l)
    loop1 s' c
    where
        catchDisconnect e =
            case fromException e of
                Just WS.ConnectionClosed -> liftIO $ modifyMVar_ s' $ \s -> do
                    let i = counter s
                    let l = removeClient c (clients s)
                    let t = FB.name u `mappend` " disconnected"
                    liftIO $ T.putStrLn t
                    broadcast t l
                    return (i,l)
                _ -> return ()

loop2 ::  JS.AcidState JS.KeyValue -> FB.User -> WS.WebSockets WS.Hybi10 ()
loop2 a' u' = do
    JS.read' a' (FB.uid u') >>= WS.sendTextData
    WS.receiveData >>= JS.write' a' (FB.uid u')
    loop2 a' u'

loop3 :: String -> WS.WebSockets WS.Hybi10 ()
loop3 p = do
    f' <- liftIO ( try $ B.readFile p :: IO (Either SomeException B.ByteString) )
    case f' of
        Right f -> WS.sendBinaryData f
        Left _ -> return ()
    WS.receiveData >>= liftIO . B.writeFile p
    loop3 p

login :: MVar Clients -> JS.AcidState JS.KeyValue -> WS.Request -> WS.WebSockets WS.Hybi10 ()
login s' a' r' = flip WS.catchWsError catchDisconnect $ do
    WS.acceptRequest r'
    WS.getVersion >>= liftIO . putStrLn . ("Connection Open: " ++)
    WS.receiveData >>= \m -> do
        u' <- liftIO (try $ FB.object (codePrefix, f m) (FB.Id "me") :: IO (Either SomeException FB.User))
        case u' of
            Right u -> do
                k <- WS.getSink
                let c = (u, k)
                WS.sendTextData ("Facebook Uid " `mappend` FB.uid u)
                case request of
                    "/chat" -> do
                        WS.sendTextData ("Facebook Name " `mappend` FB.name u)
                        liftIO $ modifyMVar_ s' $ \s -> do
                            WS.sendSink k $ WS.textData $ "Facebook Users " `mappend` T.intercalate ", " (map (FB.name . fst) (clients s))
                            let i = counter s
                            let l = addClient c (clients s)
                            let t = FB.name(fst c) `mappend` " joined"
                            T.putStrLn t
                            broadcast t l
                            return (i,l)
                        loop1 s' c
                    "/acid" -> loop2 a' u
                    "/data" -> loop3 ("data/image/"++T.unpack(FB.uid u)++".png")
                    _ -> WS.sendTextData err
            Left _ -> FB.url >>= \url -> WS.sendTextData ("Facebook Login " `mappend` url)
        where
            catchDisconnect e =
                case fromException e of
                    Just WS.ConnectionClosed -> liftIO $ putStrLn "Connection Closed"
                    _ -> return ()
            prefix = C.pack "Facebook Code "
            f=C.drop (C.length prefix)
            codePrefix= C.pack "code"
            request = C.unpack(WS.requestPath r')
            err= C.pack("Unkown Request "++request)

main :: IO ()
main = do
    putStrLn "http://localhost:9160/chat.htm"
    createDirectoryIfMissing False "data"
    createDirectoryIfMissing False "data/image"
    chat <- newMVar (0,[])
    acid <- JS.open'
    let s = defaultSettings { settingsPort = 9160, settingsIntercept = intercept (login chat acid) }
    runSettings s $ staticApp $ defaultWebAppSettings "www"
    JS.close' acid

{-
 - import Control.Concurrent (forkIO)
 -
 - WS.runServer "0.0.0.0" 9160 $ login chat acid
 -
 - import Happstack.Server (ServerPart, Response, Browsing(EnableBrowsing), simpleHTTP, nullConf, serveDirectory)
 -
 - fileServing :: ServerPart Response
 - fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"
 -
 - conf :: Conf
 - conf = Conf { port = 8000
 -             , validator = Nothing
 -             , logAccess = Just logMAccess
 -             , timeout = 30}
 -
 - simpleHTTP nullConf fileServing
 -}

