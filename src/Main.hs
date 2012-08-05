module Main where

import Chat
import Login
import Web
import Db

main :: IO ()
main = do
    chatServer
    webServer
    --couchServer
    --couchTest
    --fbTest
