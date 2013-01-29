{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Database (AcidState, KeyValue, read', write', open', close') where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens ((?=), at, from, makeIso, view)
import Data.Maybe (fromMaybe)
import Data.Aeson ((.:), (.=), Value(Object), FromJSON(parseJSON), ToJSON(toJSON), object, decode, encode)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text, unpack)
--import qualified Data.Text.Lazy.Builder as TL (toLazyText)
--import qualified Data.Text.Lazy.Internal as TL (Text)
--import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)
import Data.Acid (AcidState, Update, Query, openLocalStateFrom, makeAcidic)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose, createArchive)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)
import qualified Data.Map as Map (Map, empty)

data User = User {city::Text
                 ,country::Text
                 ,phone::Text
                 ,email::Text} deriving Typeable

newtype KeyValue = KeyValue (Map.Map String User)

$(deriveJSON id ''User)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''KeyValue)
$(makeIso ''KeyValue)

insertKey :: String -> User -> Update KeyValue ()
insertKey k v = from keyValue.at k?=v

lookupKey :: String -> Query KeyValue (Maybe User)
lookupKey k = view (from keyValue.at k)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

read' :: MonadIO m => AcidState KeyValue -> Text -> m BL.ByteString
read' s' k' = do
    let k = unpack k'
    u' <- query' s' (LookupKey k)
    case u' of
        Just u -> return $ encode u
        Nothing -> return $ encode (User "" "" "" "")

write' :: MonadIO m => AcidState KeyValue -> Text -> BL.ByteString -> m ()
write' s' k' v' = do
    let k = unpack k'
    let v = fromMaybe (error "invalid json") (decode v')
    update' s' (InsertKey k v)

open' :: MonadIO m => m (AcidState KeyValue)
open' = liftIO $ openLocalStateFrom "data/KeyValue" (KeyValue Map.empty)

close' :: MonadIO m => AcidState KeyValue -> m ()
close' s' = do
    liftIO $ createCheckpointAndClose s'
    liftIO $ createArchive s'

{-
import Data.Aeson.Encode (fromValue)

where f = decode . TL.encodeUtf8
where f = TL.toLazyText . fromValue . toJSON

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

keyValue :: Simple Iso (Map.Map Key User) KeyValue
keyValue = iso KeyValue getKeyValue

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "city"
                                <*> v .: "country"
                                <*> v .: "phone"
                                <*> v .: "email"

instance ToJSON User where
    toJSON (User a b c d)= object ["city"    .= a
                                  ,"country" .= b
                                  ,"phone"   .= c
                                  ,"email"   .= d]
-}

