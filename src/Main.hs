module Main (
  main
) where

import Chat
import Login
import Db

main :: IO ()
main = do
    chatServer
    --couchTest
    --fbTest
