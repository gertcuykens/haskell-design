module File (save,load) where

import qualified Data.Text.Lazy.Internal as L (Text)
import Login (UserId)

--path :: UserId -> FileName
--path = u + f

--p :: FilePath
--p = "test/test.txt"

--f :: Png
--f = "hello"

save :: FilePath -> String -> IO ()
save = writeFile

load :: FilePath -> IO String
load = readFile
