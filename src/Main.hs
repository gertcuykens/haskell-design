{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, newMVar, MVar, modifyMVar_, readMVar)
import System.Environment
import System.IO
import System.Exit
import System.Directory
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Internal as L (Text)
import qualified Data.Text.Lazy.IO as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as B
import Happstack.Server
import qualified Network.WebSockets as WS
import qualified Json as JS
import qualified Login as FB

type Counter = Int
type Client = (FB.User, WS.Sink WS.Hybi10)
type ChatState = (Counter,[Client])

inc :: Counter -> Counter
inc i = i + 1

counter :: ChatState -> Counter
counter (a,_) = a

clients :: ChatState -> [Client]
clients (_,b) = b

clientExists :: Client -> [Client] -> Bool
clientExists c = any ((== fst c) . fst)

addClient :: Client -> [Client] -> [Client]
addClient c cs = c : cs

removeClient :: Client -> [Client] -> [Client]
removeClient c = filter ((/= fst c) . fst)

broadcast :: T.Text -> [Client] -> IO ()
broadcast msg cs = do
    T.putStrLn msg
    forM_ cs $ \(_, sink) -> WS.sendSink sink $ WS.textData msg

loop1 :: MVar ChatState -> Client -> WS.WebSockets WS.Hybi10 ()
loop1 state c@(u,_) = flip WS.catchWsError catchDisconnect $ do
    msg <- WS.receiveData
    s <- liftIO $ readMVar state
    let i = inc (counter s)
    let cs = clients s
    liftIO $ broadcast ( T.pack(show i) `mappend` " " `mappend` FB.name u `mappend` ": " `mappend` msg) cs
    liftIO $ modifyMVar_ state $ \s -> return (i,cs)
    loop1 state c
    where
        catchDisconnect e =
            case fromException e of
                Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
                    let i = counter s
                    let cs = clients s
                    let s' = removeClient c cs
                    broadcast (FB.name u `mappend` " disconnected") s'
                    return (i,s')
                _ -> return ()

loop2 :: JS.JsState -> FB.User -> WS.WebSockets WS.Hybi10 ()
loop2 state u = do
    s <- liftIO $ JS.jsread state $ FB.uid u
    WS.sendTextData (s)
    msg <- WS.receiveData
    liftIO $ L.putStrLn msg
    liftIO $ JS.jswrite state (FB.uid u) msg
    loop2 state u

loop3 :: String -> WS.WebSockets WS.Hybi10 ()
loop3 p = do
    f <- liftIO $ B.readFile p
    WS.sendBinaryData (f)
    m <- WS.receiveData
    liftIO $ B.writeFile p m
    loop3 p

catchDisconnect :: SomeException -> WS.WebSockets WS.Hybi10 ()
catchDisconnect e =
    case fromException e of
        Just WS.ConnectionClosed -> liftIO $ putStrLn "Connection Closed"
        _ -> return ()

login :: MVar ChatState -> JS.JsState -> WS.Request -> WS.WebSockets WS.Hybi10 ()
login cstate state rq = flip WS.catchWsError catchDisconnect $ do
    WS.acceptRequest rq
    WS.getVersion >>= liftIO . putStrLn . ("Client Version: " ++)
    sink <- WS.getSink
    msg <- WS.receiveData
    liftIO $ C.putStrLn msg
    let prefix = C.pack "Facebook Code "
    let code = C.drop (C.length prefix) msg
    u <- liftIO (try $ FB.usr  (C.pack "code", code) :: IO (Either SomeException FB.User))
    case u of
        Right u -> do
            WS.sendTextData ("Facebook Uid " `mappend` T.pack(FB.uid u))
            let r = C.unpack(WS.requestPath rq)
            case r of
                "/chat" -> do
                    WS.sendTextData ("Facebook Name " `mappend` FB.name u)
                    s <- liftIO $ readMVar cstate
                    let i = counter s
                    let c = (u, sink)
                    let cs = clients s
                    --if clientExists c cs
                    --    then WS.sendTextData ("User already exists" :: Text)
                    --    else do
                    liftIO $ modifyMVar_ cstate $ \s -> do
                        let s' = addClient c cs
                        WS.sendSink sink $ WS.textData $ "Facebook Users " `mappend` T.intercalate ", " (map (FB.name . fst) cs)
                        broadcast (FB.name(fst c) `mappend` " joined") s'
                        return (i,s')
                    loop1 cstate c
                "/acid" -> loop2 state u
                "/data" -> do
                    liftIO $ createDirectoryIfMissing False ("data/" `mappend` FB.uid u)
                    loop3 ("data/"++FB.uid u++"/picture.png")
                _ -> WS.sendTextData (C.pack("Unkown Request "++r))
        Left _ -> do
            url <- liftIO FB.url
            WS.sendTextData ("Facebook Login " `mappend` url)
            --WS.sendTextData (C.pack("Facebook Login " ++ T.unpack url))
            --liftIO $ print $ "error: " ++ show (e :: SomeException)

conf :: Conf
conf = Conf { port      = 8000
            , validator = Nothing
            , logAccess = Just logMAccess
            , timeout   = 30}

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"

main :: IO ()
main = do
    chat <- newMVar (0,[])
    acid <- JS.jsopen
    createDirectoryIfMissing False "data"
    forkIO $ WS.runServer "0.0.0.0" 9160 $ login chat acid
    print "Starting http://localhost:8000"
    simpleHTTP nullConf fileServing
    JS.jsclose acid
