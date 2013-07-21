{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main where
import Control.Arrow ((***))
--import Control.Concurrent (forkIO)
import Control.Concurrent (newMVar, MVar, modifyMVar_, readMVar)
import Control.Exception (bracket, SomeException, try, fromException)
import Control.Lens (perform, traverse, act, _2, (?=), at, from, makeIso, view)
import Control.Monad (forever, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Class (lift)
import Data.Acid (AcidState, closeAcidState, createCheckpoint, Update, Query, makeAcidic)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (openLocalState, createCheckpointAndClose, createArchive)
import Data.Acid.Remote (openRemoteState, sharedSecretPerform)
import Data.Aeson (Value(Object), FromJSON(parseJSON), ToJSON(toJSON), object, decode, encode, json, fromJSON, Result)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
--import Data.Conduit.Network (Application)
import Data.Data (Data)
import Data.Function.Pointless ((.:))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Monoid (mappend)
import Data.String (fromString)
import Data.Text (Text, unpack, pack, intercalate)
import Data.Typeable (Typeable)
import qualified Google
import Network (PortID(..))
import Network.HTTP.Conduit (def, newManager) -- Response(..))
--import Network.HTTP.ReverseProxy  (WPRProxyDest)
import Network.HTTP.ReverseProxy  (ProxyDest(..), waiProxyToSettings, defaultOnExc, waiProxyTo, waiProxyToSettings, wpsTimeout, wpsOnExc)
import Network.HTTP.Types (status200)
import Network.Mime (MimeMap, defaultMimeMap, mimeByExt, defaultMimeType)
import Network.Wai (Application, Middleware, Request(..), Response, responseLBS)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp (runSettings, defaultSettings, settingsIntercept, settingsHost, settingsPort)
--import Network.Wai.Handler.WarpTLS (TLSSettings(..), runTLS)
import Network.Wai.Handler.WebSockets (intercept)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Gzip
import qualified Network.WebSockets as WS
import System.Console.CmdArgs (cmdArgs)
import System.Directory (createDirectoryIfMissing, canonicalizePath)
import Text.Printf (printf)
import Table (UserMap(..), User(..), InsertKey(..), LookupKey(..))
import WaiAppStatic.Types (ssIndices, toPiece, ssGetMimeType, fileName, fromPiece)
--import Prelude hiding (id)
--import qualified Prelude as P (id)

$(deriveJSON id ''User)

read' :: MonadIO m => AcidState UserMap -> Text -> m BL.ByteString
read' s' k' = do
    let k = unpack k'
    u' <- query' s' (LookupKey k)
    case u' of
        Just u -> return $ encode u
        Nothing -> return $ encode (User "" "" "" "")

write' :: MonadIO m => AcidState UserMap -> Text -> BL.ByteString -> m ()
write' s' k' v' = do
    let k = unpack k'
    let v = fromMaybe (error "invalid json") (decode v')
    update' s' (InsertKey k v)

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    , quiet :: Bool
    , verbose :: Bool
    , mime :: [(String, String)]
    , host :: String
    } deriving (Show, Typeable, Data)

defaultArgs :: Args
defaultArgs = Args "www" ["main.htm"] 9160 False False False [] "*"

--defaultTLS :: TLSSettings
--defaultTLS = TLSSettings "ssl/cert.pem" "ssl/key.pem"

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

loop2 ::  AcidState UserMap -> Text -> WS.WebSockets WS.Hybi10 ()
loop2 state uid = forever $ do
    read' state uid >>= WS.sendTextData
    WS.receiveData >>= write' state uid

loop3 :: String -> WS.WebSockets WS.Hybi10 ()
loop3 p = forever $ do
    f' <- liftIO ( try $ BL.readFile p :: IO (Either SomeException BL.ByteString) )
    case f' of
        Right f -> WS.sendBinaryData f
        Left _ -> return ()
    WS.receiveData >>= liftIO . BL.writeFile p

login :: MVar Clients -> AcidState UserMap -> WS.Request -> WS.WebSockets WS.Hybi10 ()
login s' a' r' = flip WS.catchWsError catchDisconnect $ do
    WS.acceptRequest r'
    --WS.getVersion >>= liftIO . print . ("Connection Open: " ++)
    WS.receiveData >>= \m ->
        --liftIO (print (BS.unpack m)) >>
        case request of
            "/code" -> liftIO (Google.token m) >>= WS.sendTextData
            "/acid" -> liftIO (Google.userinfo' m) >>= \(Just (Google.User a _ _ _ _ _ _ _ _)) -> loop2 a' a
            "/chat" -> liftIO (Google.userinfo' m) >>= \(Just u@(Google.User _ b _ _ _ _ _ _ _)) -> do
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
            "/image" -> liftIO (Google.userinfo' m) >>= \(Just (Google.User a _ _ _ _ _ _ _ _)) -> loop3 ("image/"++unpack a++".png")
            _ -> WS.sendTextData err
        where
            catchDisconnect e =
                case fromException e of
                    Just WS.ConnectionClosed -> liftIO $ putStrLn "Connection Closed"
                    _ -> return ()
            request = BS.unpack(WS.requestPath r')
            err= BS.pack("Unkown Request "++request)

--modReq :: Request -> ResourceT IO (Either Response ProxyDest)
--modReq pdest req = return $ WPRModifiedRequest
--       (req { rawPathInfo = "/v1/AUTH_a4c2ef109c996858a5c3f8e411de8538/gert/index.html" })
--       pdest

proxy1 :: Application
proxy1 req = do
    manager <- lift $ newManager def
    waiProxyTo (const $ return $ Right $ ProxyDest "lb1.pcs.ovh.net" 443) defaultOnExc manager req

--proxy2 :: Request -> ResourceT IO Response
proxy2 :: Application
proxy2 req = do
    manager <- lift $ newManager def
    waiProxyToSettings
        (const $ return $ Right $ ProxyDest "lb1.pcs.ovh.net" 443)
        def { wpsOnExc = onExc, wpsTimeout = Nothing}
        manager req
    where
        onExc _ _ = return $ responseLBS
            status200
            [ ("content-type", "text/html")
            , ("Refresh", "1")
            ]
            "<h1>App not ready, please refresh</h1>"
            --L8.fromChunks [rawPathInfo req]

static :: Args -> Application
static arg =
    let Args {..} = arg
        mime' = map (pack *** BS.pack) mime
        mimeMap = Map.fromList mime' `Map.union` defaultMimeMap
        middle = gzip def . (if verbose then logStdout else id) . autohead in
    middle $ staticApp (defaultFileServerSettings $ fromString docroot)
        { ssIndices = if noindex then [] else mapMaybe (toPiece . pack) index
        , ssGetMimeType = return . mimeByExt mimeMap defaultMimeType . fromPiece . fileName
        }


server :: AcidState UserMap -> IO ()
server acid = do
    createArchive acid
    createDirectoryIfMissing False "image"
    chat <- newMVar (0,[])
    arg@Args {..} <- cmdArgs defaultArgs
    docroot' <- canonicalizePath docroot
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\nhttp://localhost:9160\n" docroot' port (if noindex then "no" else show index)
    runSettings defaultSettings
        { settingsPort = port
        , settingsHost = fromString host
        , settingsIntercept = intercept (login chat acid)
        } $ static arg

main :: IO ()
main = bracket
    --(openRemoteState (sharedSecretPerform $ BS.pack "12345") "localhost" (PortNumber 8080))
    (openLocalState (UserMap Map.empty))
    createCheckpointAndClose
    server

{---------------snap-----------------------
 - import qualified Network.WebSockets.Snap as WS
 - import Snap.Http.Server (httpServe, setAccessLog, setErrorLog, setPort, ConfigLog(..))
 - import Snap.Util.FileServe (serveDirectory)
 - let config = setErrorLog ConfigNoLog $ setAccessLog ConfigNoLog $ setPort 8000 mempty
 - httpServe config $ serveDirectory $ fromString docroot
 ---------------ws-------------------------
 - WS.runServer "0.0.0.0" 9160 $ login chat acid
 ---------------happstack------------------
 - import Happstack.Server (ServerPart, Response, Browsing(EnableBrowsing), simpleHTTP, nullConf, serveDirectory)
 - fileServing :: ServerPart Response
 - fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"
 - conf :: Conf
 - conf = Conf { port = 8000, validator = Nothing, logAccess = Just logMAccess, timeout = 30}
 - simpleHTTP nullConf fileServing
 ------------------------------------------}

