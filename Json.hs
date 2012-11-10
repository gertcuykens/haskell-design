{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Json (AcidState, KeyValue, read', write', open', close') where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Aeson ((.:), (.=), Value(Object), FromJSON(parseJSON), ToJSON(toJSON), object, decode)
import Data.Aeson.Encode (fromValue)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Acid (AcidState, Update, Query, update, query, openLocalStateFrom, closeAcidState, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)
import qualified Data.Map as Map (Map, lookup, insert, empty)

type Key = String
data User = User Text Text Text Text deriving (Typeable)
data KeyValue = KeyValue !(Map.Map Key User) deriving (Typeable)

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''KeyValue)

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "city"
                                <*> v .: "country"
                                <*> v .: "phone"
                                <*> v .: "email"
    parseJSON _ = mzero

instance ToJSON User where
    toJSON (User a b c d)= object ["city"    .= a
                                  ,"country" .= b
                                  ,"phone"   .= c
                                  ,"email"   .= d]

insertKey :: Key -> User -> Update KeyValue ()
insertKey k v = do
    KeyValue m <- get
    put (KeyValue (Map.insert k v m))

lookupKey :: Key -> Query KeyValue (Maybe User)
lookupKey k = do
    KeyValue m <- ask
    return (Map.lookup k m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

read' :: AcidState KeyValue -> String -> IO Text
read' s' k = do
    u' <- query s' (LookupKey k)
    case u' of
        Just u ->return $ f u
        Nothing -> return $ f (User "" "" "" "")
        where f = toLazyText . fromValue . toJSON

write' :: AcidState KeyValue -> String -> Text -> IO ()
write' s' k v = do
    let u = fromMaybe (error "invalid json") (f v)
    update s' (InsertKey k u)
    where f = decode . encodeUtf8

open' :: IO (AcidState KeyValue)
open' = openLocalStateFrom "data/KeyValue" (KeyValue Map.empty)

close' :: AcidState KeyValue -> IO ()
close' = closeAcidState

