module Main where

import Control.Concurrent (forkIO)
import Network.WebSockets (runServer)
import Happstack.Server
import Chat (chat)
import User (user)
import Picture (picture)
import File

conf :: Conf
conf = Conf { port      = 8000
            , validator = Nothing
            , logAccess = Just logMAccess
            , timeout   = 30}

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"

main :: IO ()
main = do
    mkdir "data"
    print "Starting http://localhost:8000"
    forkIO $ runServer "0.0.0.0" 9160 $ user
    forkIO $ runServer "0.0.0.0" 9161 $ picture
    forkIO $ runServer "0.0.0.0" 9162 $ chat
    simpleHTTP nullConf fileServing
