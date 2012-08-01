module Main (
  main
) where

import Server
import Login
import qualified Data.ByteString.Char8 as C
import Control.Exception

main :: IO ()
main = do
    websocket

    --u <- fbUrl
    --print u

    --a <- readLn
    --e <- fbEmail a

    --let a = ("code","test")
    --e <- try (fbEmail $ (\(x,y) -> (C.pack x, C.pack y)) a)

    --case e of
    --    Left x -> print "doh!"
    --    Right e -> print "ok"

        --(Just e)
        --Nothing -> print "doh!"
        --Just e -> print e

--e <- fbEmail $ (\(x,y) -> (C.pack x, C.pack y)) a
--http://book.realworldhaskell.org/read/error-handling.html

