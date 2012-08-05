module Main where

import Control.Concurrent
import Chat
import Login
import Web
import Db

main :: IO ()
main = do
    forkIO chatServer
    webServer
    --couchServer
    --couchTest
    --fbTest
