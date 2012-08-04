module Main (
  main
) where

import Server
import Login
import Db

main :: IO ()
main = do
    websocket
    --couchTest
    --fbTest
