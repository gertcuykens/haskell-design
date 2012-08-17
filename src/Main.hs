module Main where
import Control.Concurrent (forkIO)
import Chat
import User
import Web

main :: IO ()
main = do
    print "Starting http://localhost:8000"
    forkIO chatServer
    forkIO jsonServer
    --forkIO fileServer
    webServer
    print "Closed"
