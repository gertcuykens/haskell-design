module Main where

import Control.Concurrent (forkIO)
import Chat
import User
import Picture
import Web

import File

import Login
import qualified Data.ByteString.Char8 as C (pack)
import Control.Exception

main :: IO ()
main = do
    print "Starting http://localhost:8000"
    forkIO chatServer
    forkIO jsonServer
    forkIO fileServer
    webServer

----------------------------------
--           Test               --
----------------------------------

file :: IO ()
file = do
    save "test/test.txt" "hello2"
    f <- load "test/test.txt"
    print f

login :: IO ()
login = do
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
