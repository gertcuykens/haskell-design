{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards, DeriveDataTypeable #-}
module Main where

import Control.Arrow ((***))
import Control.Exception (SomeException, try, fromException)
import Control.Lens (perform, traverse, act, _2)
import Control.Monad (forever)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (newMVar, MVar, modifyMVar_, readMVar)
import System.Console.CmdArgs hiding (def)
import System.Directory (createDirectoryIfMissing, canonicalizePath)
--import Data.Char (isPunctuation, isSpace)
--import Data.Aeson (encode, decode)
--import Data.Aeson.TH (deriveJSON)
import Data.Monoid (mappend)
import Data.Function.Pointless ((.:))
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.String (fromString)
import Data.Text (Text, unpack, pack, intercalate)
--import qualified Data.Text.IO as T
--import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Lazy.Encoding as TL
--import Network.HTTP.Conduit (Response(..))
import Network.Mime (defaultMimeMap, mimeByExt, defaultMimeType)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsPort)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsHost, settingsPort)
--import Network.Wai.Handler.WarpTLS (TLSSettings, runTLS)
import Network.Wai.Handler.WebSockets (intercept)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Gzip
import qualified Network.WebSockets as WS
import Text.Printf (printf)
import WaiAppStatic.Types (ssIndices, toPiece, ssGetMimeType, fileName, fromPiece)
import qualified Database as DB
import qualified Google as Google

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    , quiet :: Bool
    , verbose :: Bool
    , mime :: [(String, String)]
    , host :: String
    } deriving (Show, Data, Typeable)

type Counter = Int
type Client = (Google.User, WS.Sink WS.Hybi10)
type Clients = (Counter, [Client])

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
--removeClient c = filter ((/= snd c) . snd)

broadcast :: MonadIO m => Text -> [Client] -> m ()
broadcast t = liftIO .: perform $ traverse._2.act (`WS.sendSink` WS.textData t)
--broadcast t = (liftIO .) . perform $ traverse._2.act (`WS.sendSink` WS.textData t)
--broadcast t l = liftIO $ T.putStrLn t >> l ^! traverse . _2 . act (`WS.sendSink` WS.textData t)
--broadcast t l = liftIO (T.putStrLn t) >> liftIO (forM_ l $ \(_, k) -> WS.sendSink k $ WS.textData t)

loop1 :: MVar Clients -> Client -> WS.WebSockets WS.Hybi10 ()
loop1 s' c@(u,_) = flip WS.catchWsError catchDisconnect $
    forever $ do
        m <- WS.receiveData
        s <- liftIO $ readMVar s'
        let i = counter s + 1
        let l = clients s
        let t = pack(show i) `mappend` " " `mappend` Google.name u `mappend` ": " `mappend` m
        liftIO $ print t
        broadcast t l
        liftIO $ modifyMVar_ s' $ \_ -> return (i,l)
    where
        catchDisconnect e =
            case fromException e of
                Just WS.ConnectionClosed -> liftIO $ modifyMVar_ s' $ \s -> do
                    let i = counter s
                    let l = removeClient c (clients s)
                    let t = Google.name u `mappend` " disconnected"
                    liftIO $ print t
                    broadcast t l
                    return (i,l)
                _ -> return ()

loop2 ::  DB.AcidState DB.KeyValue -> Text -> WS.WebSockets WS.Hybi10 ()
loop2 state uid = forever $ do
    DB.read' state uid >>= WS.sendTextData
    WS.receiveData >>= DB.write' state uid

loop3 :: String -> WS.WebSockets WS.Hybi10 ()
loop3 p = forever $ do
    f' <- liftIO ( try $ BL.readFile p :: IO (Either SomeException BL.ByteString) )
    case f' of
        Right f -> WS.sendBinaryData f
        Left _ -> return ()
    WS.receiveData >>= liftIO . BL.writeFile p

login :: MVar Clients -> DB.AcidState DB.KeyValue -> WS.Request -> WS.WebSockets WS.Hybi10 ()
login s' a' r' = flip WS.catchWsError catchDisconnect $ do
    WS.acceptRequest r'
    --WS.getVersion >>= liftIO . print . ("Connection Open: " ++)
    WS.receiveData >>= \m -> do
        --liftIO $ print (BS.unpack m)
        case request of
            "/code" -> do
                t <- liftIO $ Google.token m
                WS.sendTextData (t)
            "/acid" -> do
                Just (Google.User a b c d e f g h i) <- liftIO $ Google.userinfo' m
                loop2 a' a
            "/chat" -> do
                Just u@(Google.User a b c d e f g h i) <- liftIO $ Google.userinfo' m
                WS.sendTextData ("Facebook Name " `mappend` b)
                k <- WS.getSink
                liftIO $ modifyMVar_ s' $ \s -> do
                    WS.sendSink k $ WS.textData $ "Facebook Users " `mappend` intercalate ", " (map (Google.name . fst) (clients s))
                    let i = counter s
                    let l = addClient (u,k) (clients s)
                    let t = b `mappend` " joined"
                    print t
                    broadcast t l
                    return (i,l)
                loop1 s' (u,k)
            "/data" -> do
                 Just (Google.User a b c d e f g h i) <- liftIO $ Google.userinfo' m
                 loop3 ("data/image/"++unpack a++".png")
            _ -> WS.sendTextData err
        where
            catchDisconnect e =
                case fromException e of
                    Just WS.ConnectionClosed -> liftIO $ putStrLn "Connection Closed"
                    _ -> return ()
            prefix = BS.pack "Facebook Code "
            f= BS.drop (BS.length prefix)
            codePrefix= BS.pack "code"
            request = BS.unpack(WS.requestPath r')
            err= BS.pack("Unkown Request "++request)

defaultArgs :: Args
defaultArgs = Args "www" ["index.html", "index.htm"] 9160 False False False [] "*"

main :: IO ()
main = do
    Args {..} <- cmdArgs defaultArgs
    let mime' = map (pack *** BS.pack) mime
    let mimeMap = Map.fromList mime' `Map.union` defaultMimeMap
    docroot' <- canonicalizePath docroot
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\nhttp://localhost:9160\n" docroot' port (if noindex then "no" else show index)
    let middle = gzip def
               . (if verbose then logStdout else id)
               . autohead
    createDirectoryIfMissing False "data"
    createDirectoryIfMissing False "data/image"
    chat <- newMVar (0,[])
    acid <- DB.open'
    --let s = defaultSettings { settingsPort = 9160, settingsIntercept = intercept (login chat acid) }
    --runSettings s $ staticApp $ defaultWebAppSettings "www"
    runSettings defaultSettings
        { settingsPort = port
        , settingsHost = fromString host
        , settingsIntercept = intercept (login chat acid)
        } $ middle $ staticApp (defaultFileServerSettings $ fromString docroot)
        { ssIndices = if noindex then [] else mapMaybe (toPiece . pack) index
        , ssGetMimeType = return . mimeByExt mimeMap defaultMimeType . fromPiece . fileName
        }
    DB.close' acid

{------------------------------------------
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
 ------------------------------------------}

