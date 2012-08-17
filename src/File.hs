module File () where

import Login (UserId)
import Picture (Png,FileName)

--path :: UserId -> FileName
--path = u + f

--p :: FilePath
--p = "test/test.txt"

--f :: Png
--f = "hello"

save :: FilePath -> Png -> IO ()
save p _ = writeFile p ""
save p f = writeFile p f

load :: FilePath -> IO ()
load p = readFile
