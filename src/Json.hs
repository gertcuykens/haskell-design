{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Json (AcidState, KeyValue, read', write', open', close') where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens ((?=), at, from, makeIso, peruse)
import Data.Maybe (fromMaybe)
import Data.Aeson ((.:), (.=), Value(Object), FromJSON(parseJSON), ToJSON(toJSON), object, decode)
import Data.Aeson.Encode (fromValue)
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy.Builder as L (toLazyText)
import qualified Data.Text.Lazy.Internal as L (Text)
import qualified Data.Text.Lazy.Encoding as L (encodeUtf8)
import Data.Acid (AcidState, Update, Query, openLocalStateFrom, makeAcidic)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose, createArchive)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)
import qualified Data.Map as Map (Map, empty)

type Key = String
data User = User Text Text Text Text deriving Typeable
newtype KeyValue = KeyValue (Map.Map Key User)

$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''KeyValue)
$(makeIso ''KeyValue)

insertKey :: Key -> User -> Update KeyValue ()
insertKey k v = from keyValue.at k?=v

lookupKey :: Key -> Query KeyValue (Maybe User)
lookupKey k = peruse (from keyValue.at k)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

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

read' :: MonadIO m => AcidState KeyValue -> Text -> m L.Text
read' s' k' = do
    let k = unpack k'
    u' <- query' s' (LookupKey k)
    case u' of
        Just u ->return $ f u
        Nothing -> return $ f (User "" "" "" "")
        where f = L.toLazyText . fromValue . toJSON

write' :: MonadIO m => AcidState KeyValue -> Text -> L.Text -> m ()
write' s' k' v = do
    let k = unpack k'
    let u = fromMaybe (error "invalid json") (f v)
    update' s' (InsertKey k u)
    where f = decode . L.encodeUtf8

open' :: MonadIO m => m (AcidState KeyValue)
open' = liftIO $ openLocalStateFrom "data/KeyValue" (KeyValue Map.empty)

close' :: MonadIO m => AcidState KeyValue -> m ()
close' s' = do
    liftIO $ createCheckpointAndClose s'
    liftIO $ createArchive s'

{-
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)

insertKey :: Key -> User -> Update KeyValue ()
insertKey k v = do
    KeyValue m <- get
    put (KeyValue (Map.insert k v m))

lookupKey :: Key -> Query KeyValue (Maybe User)
lookupKey k = do
    KeyValue m <- ask
    return (Map.lookup k m)
-}

{-
keyValue :: Simple Iso (Map.Map Key User) KeyValue
keyValue = iso KeyValue getKeyValue
-}

