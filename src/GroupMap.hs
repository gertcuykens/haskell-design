{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell#-}
module GroupMap where
import Control.Lens ((?=), at, from, makeIso, view)
import Data.Acid (Update, Query, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)
import Data.IntMap (IntMap)
import Data.Set (Set)

newtype GroupMap = GroupMap (IntMap (Set Integer)) deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''GroupMap)

$(makeIso ''GroupMap)

groupInsert :: Int -> Set Integer -> Update GroupMap ()
groupInsert k v = (from groupMap.at k) ?= v

groupLookup :: Int -> Query GroupMap (Maybe (Set Integer))
groupLookup k = view (from groupMap.at k)

$(makeAcidic ''GroupMap ['groupInsert, 'groupLookup])

