module File (save,mkdir) where

import System.Directory
import System.FilePath
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Login (UserId)

mkdir :: FilePath -> IO ()
mkdir i = createDirectoryIfMissing False i

save :: FilePath -> B.ByteString -> IO ()
save f = B.writeFile f

{-
--path :: UserId -> FileName
--path = u + f

--p :: FilePath
--p = "test/test.txt"

--f :: Png
--f = "hello"

load :: FilePath -> IO String
load = readFile
-}

