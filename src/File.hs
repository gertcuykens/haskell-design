module File () where

import Login (UserId)
import Picture (Png)

--path:: UserId -> ()
--path= u + f

save :: FilePath -> Png ->IO()
save p _ = writeFile p ""
save p f = writeFile p f
