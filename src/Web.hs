module Web (webServer) where

import Happstack.Server

conf :: Conf
conf = Conf { port      = 8000
            , validator = Nothing
            , logAccess = Just logMAccess
            , timeout   = 30}

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"

webServer :: IO ()
webServer = simpleHTTP nullConf fileServing
