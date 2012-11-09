{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception (SomeException, try, fromException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, newMVar, MVar, modifyMVar_, readMVar)
--import Control.Applicative
--import System.Environment
--import System.IO (IOMode(ReadMode), withFile, hGetLine)
--import System.Exit
import System.Directory
--import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Happstack.Server (ServerPart, Response, Browsing(EnableBrowsing), simpleHTTP, nullConf, serveDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as T
--import qualified Data.Text.Lazy.Internal as L (Text)
--import qualified Data.Text.Lazy.IO as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Network.WebSockets as WS
import qualified Json as JS
import qualified Login as FB

type Counter = Int
type Client = (FB.User, WS.Sink WS.Hybi10)
type Clients = (Counter,[Client])

inc :: Counter -> Counter
inc i = i + 1

counter :: Clients -> Counter
counter (a,_) = a

clients :: Clients -> [Client]
clients (_,b) = b

clientExists :: Client -> [Client] -> Bool
clientExists c = any ((== fst c) . fst)

addClient :: Client -> [Client] -> [Client]
addClient c l = c : l

removeClient :: Client -> [Client] -> [Client]
removeClient c = filter ((/= fst c) . fst)

broadcast :: T.Text -> [Client] -> IO ()
broadcast m l = do
    T.putStrLn m
    forM_ l $ \(_, k) -> WS.sendSink k $ WS.textData m

loop1 :: MVar Clients -> Client -> WS.WebSockets WS.Hybi10 ()
loop1 s' c@(u,_) = flip WS.catchWsError catchDisconnect $ do
    m <- WS.receiveData
    s <- liftIO $ readMVar s'
    let i = inc (counter s)
    let l = clients s
    liftIO $ broadcast ( T.pack(show i) `mappend` " " `mappend` FB.name u `mappend` ": " `mappend` m) l
    liftIO $ modifyMVar_ s' $ \_ -> return (i,l)
    loop1 s' c
    where
        catchDisconnect e =
            case fromException e of
                Just WS.ConnectionClosed -> liftIO $ modifyMVar_ s' $ \s -> do
                    let i = counter s
                    let l = removeClient c (clients s)
                    broadcast (FB.name u `mappend` " disconnected") l
                    return (i,l)
                _ -> return ()

loop2 ::  JS.AcidState JS.KeyValue -> FB.User -> WS.WebSockets WS.Hybi10 ()
loop2 a' u' = do
    a <- liftIO $ JS.read' a' $ FB.uid u'
    WS.sendTextData a
    WS.receiveData >>= liftIO . JS.write' a' (FB.uid u')
    loop2 a' u'

loop3 :: String -> WS.WebSockets WS.Hybi10 ()
loop3 p = do
    f' <- liftIO ( try $ B.readFile p :: IO (Either SomeException B.ByteString) )
    case f' of
        Right f -> WS.sendBinaryData f
        Left _ -> return ()
    m <- WS.receiveData
    liftIO $ B.writeFile p m
    loop3 p

login :: MVar Clients -> JS.AcidState JS.KeyValue -> WS.Request -> WS.WebSockets WS.Hybi10 ()
login s' a' r' = flip WS.catchWsError catchDisconnect $ do
    WS.acceptRequest r'
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    k <- WS.getSink
    m <- WS.receiveData
    liftIO $ C.putStrLn m
    let r = C.unpack(WS.requestPath r')
    let prefix = C.pack "Facebook Code "
    let code = C.drop (C.length prefix) m
    u' <- liftIO (try $ FB.usr  (C.pack "code", code) :: IO (Either SomeException FB.User))
    case u' of
        Right u -> do
            let c = (u, k)
            WS.sendTextData ("Facebook Uid " `mappend` T.pack(FB.uid u))
            case r of
                "/chat" -> do
                    WS.sendTextData ("Facebook Name " `mappend` FB.name u)
                    liftIO $ modifyMVar_ s' $ \s -> do
                        -- if clientExists c (clients s) then WS.sendTextData ("User already exists" :: Text) else do
                        WS.sendSink k $ WS.textData $ "Facebook Users " `mappend` T.intercalate ", " (map (FB.name . fst) (clients s))
                        let i = counter s
                        let l = addClient c (clients s)
                        broadcast (FB.name(fst c) `mappend` " joined") l
                        return (i,l)
                    loop1 s' c
                "/acid" -> loop2 a' u
                "/data" -> do
                    liftIO $ createDirectoryIfMissing False "data/image"
                    loop3 ("data/image/"++FB.uid u++".png")
                _ -> WS.sendTextData (C.pack("Unkown Request "++r))
        Left _ -> do
            url <- FB.url
            WS.sendTextData ("Facebook Login " `mappend` url)
            --WS.sendTextData (C.pack("Facebook Login " ++ T.unpack url))
            --liftIO $ print $ "error: " ++ show (e :: SomeException)
     where
       catchDisconnect e =
         case fromException e of
           Just WS.ConnectionClosed -> liftIO $ putStrLn "Connection Closed"
           _ -> return ()

{-
conf :: Conf
conf = Conf { port = 8000
            , validator = Nothing
            , logAccess = Just logMAccess
            , timeout = 30}
-}

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"

main :: IO ()
main = do
    chat <- newMVar (0,[])
    acid <- JS.open'
    createDirectoryIfMissing False "data"
    forkIO $ WS.runServer "0.0.0.0" 9160 $ login chat acid
    putStrLn "Starting http://localhost:8000"
    simpleHTTP nullConf fileServing
    JS.close' acid

