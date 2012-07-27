module Main (
  main
) where

import Server
import Login
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    websocket

    --u <- fbUrl
    --print u

    --a <- readLn
    --e <- fbEmail a
    --print e

    --let a = ("code","test")
    --e <- fbEmail $ (\(x,y) -> (C.pack x, C.pack y)) a
    --print e


