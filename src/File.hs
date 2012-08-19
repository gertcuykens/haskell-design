module File (save,mkdir) where

import System.Directory
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Login (UserId)

mkdir :: UserId -> IO ()
mkdir i = createDirectoryIfMissing False (show i)

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

