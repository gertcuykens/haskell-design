module Main where

import Control.Concurrent (forkIO)
import Chat
import User
import Picture
import Web

main :: IO ()
main = do
    print "Starting http://localhost:8000"
    forkIO chatServer
    forkIO jsonServer
    forkIO fileServer
    webServer
    print "Closed"

{-
fbTest :: IO ()
fbTest = do
    u <- fbUrl
    print u

    --a <- readLn
    --e <- try . fbEmail $ a

    let a = ("code","test")
    e <- try . fbEmail $ (\(x,y) -> (C.pack x, C.pack y)) a
    case e of
        Left e -> print $ "error: " ++ show (e :: SomeException)
        Right Nothing -> print "doh!"
        Right (Just e) -> print e
-}
