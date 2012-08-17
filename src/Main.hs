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
import File
test :: IO ()
test = do
    save ("test/test.txt") "hello"
    print (load "test/test.txt")

import Login
import qualified Data.ByteString.Char8 as C (pack)
test :: IO ()
test = do
    u <- url
    print u

    --a <- readLn
    --e <- try . email $ a

    let a = ("code","test")
    e <- try . email $ (\(x,y) -> (C.pack x, C.pack y)) a
    case e of
        Left e -> print $ "error: " ++ show (e :: SomeException)
        Right Nothing -> print "doh!"
        Right (Just e) -> print e
-}
