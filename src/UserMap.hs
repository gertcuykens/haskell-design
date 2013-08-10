{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell#-}
module UserMap where
import Control.Lens ((?=), at, from, makeIso, view)
import Data.Acid (Update, Query, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Text (Text)
import Data.Typeable (Typeable)
import qualified Data.Map as Map (Map)

data User = User {city::Text
                 ,country::Text
                 ,phone::Text
                 ,email::Text} deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''User)

newtype UserMap = UserMap (Map.Map String User) deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''UserMap)

$(makeIso ''UserMap)

userInsert :: String -> User -> Update UserMap ()
userInsert k v = (from userMap.at k) ?= v

userLookup :: String -> Query UserMap (Maybe User)
userLookup k = view (from userMap.at k)

$(makeAcidic ''UserMap ['userInsert, 'userLookup])

