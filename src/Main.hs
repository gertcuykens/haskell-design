module Main where
import Control.Concurrent
import Chat
import Web

main :: IO ()
main = do
    print "http://localhost:8000"
    forkIO chatServer
    webServer

