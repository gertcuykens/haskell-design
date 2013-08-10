{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell#-}
module GroupMap where
import Control.Lens ((?=), at, from, makeIso, view)
import Data.Acid (AcidState, Update, Query, query, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)
import Data.IntMap (IntMap)
import Data.Set (Set, member)

newtype GroupMap = GroupMap (IntMap (Set Integer)) deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''GroupMap)

$(makeIso ''GroupMap)

groupInsert :: Int -> Set Integer -> Update GroupMap ()
groupInsert k v = (from groupMap.at k) ?= v

groupLookup :: Int -> Query GroupMap (Maybe (Set Integer))
groupLookup k = view (from groupMap.at k)

$(makeAcidic ''GroupMap ['groupInsert, 'groupLookup])

check :: Integer -> [Int] -> AcidState GroupMap -> IO Bool
check _ [] _ = return True
check i [x] acid = query acid (GroupLookup x) >>= \(Just set) -> return (i `member` set)
check i (x:xs) acid = query acid (GroupLookup x) >>= \(Just set) -> if i `member` set then check i xs acid else return False

