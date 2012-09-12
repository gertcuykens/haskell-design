{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Main where

import System.Directory
import Control.Concurrent (forkIO,newMVar)
import Control.Monad.IO.Class (liftIO)
import Network.WebSockets (runServer)
import Happstack.Server

import Data.Acid
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import System.Environment
import System.IO
import System.Exit
import Data.SafeCopy
import Data.Typeable
import qualified Data.Map as DB

import Chat (chat)
import User (user)
import Picture (picture)

import qualified Json as JS

conf :: Conf
conf = Conf { port      = 8000
            , validator = Nothing
            , logAccess = Just logMAccess
            , timeout   = 30}

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["state.htm"] "www"

main :: IO ()
main = do
    acid <- openLocalState (JS.KeyValue DB.empty)
    createDirectoryIfMissing False "data"
    state <- liftIO $ newMVar (0,[])
    forkIO $ runServer "0.0.0.0" 9160 $ user acid
    forkIO $ runServer "0.0.0.0" 9161 $ picture "data/"
    forkIO $ runServer "0.0.0.0" 9162 $ chat state
    print "Starting http://localhost:8000"
    simpleHTTP nullConf fileServing
    closeAcidState acid
