module Main (
  main
) where

import Server

--main :: IO ()
--main = websocket

import Login

main :: IO ()
main = do
    u <- fbUrl
    print u
    a <- readLn
    e <- fbEmail a
    --let a = ("code","dddd")
    --e <- fbEmail a
    print e

