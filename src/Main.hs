module Main where

import System.Directory
import Control.Concurrent (forkIO,newMVar)
import Control.Monad.IO.Class (liftIO)
import Network.WebSockets (runServer)
import Happstack.Server
import Chat (chat)
import User (user)
import Picture (picture)

conf :: Conf
conf = Conf { port      = 8000
            , validator = Nothing
            , logAccess = Just logMAccess
            , timeout   = 30}

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"

main :: IO ()
main = do
    state0 <- liftIO $ newMVar []
    state2 <- liftIO $ newMVar (0,[])
    createDirectoryIfMissing False "data"
    print "Starting http://localhost:8000"
    forkIO $ runServer "0.0.0.0" 9160 $ user state0
    forkIO $ runServer "0.0.0.0" 9161 $ picture
    forkIO $ runServer "0.0.0.0" 9162 $ chat state2
    simpleHTTP nullConf fileServing
