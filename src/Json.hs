{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Json (AcidState, KeyValue, read', write', open', close') where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (mzero)
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)

import Data.Maybe (fromMaybe)
import Data.Aeson hiding (Value)
import Data.Aeson.Encode (fromValue)
--import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Data.Acid (AcidState, Update, Query, update, query, openLocalState, closeAcidState, makeAcidic)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)

import qualified Data.Map as Map

data User = User Text Text Text Text deriving (Typeable)

$(deriveSafeCopy 0 'base ''User)

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "city"
                                <*> v .: "country"
                                <*> v .: "phone"
                                <*> v .: "email"
    parseJSON _          = mzero

instance ToJSON User where
    toJSON (User a b c d)= object ["city"    .= a
                                  ,"country" .= b
                                  ,"phone"   .= c
                                  ,"email"   .= d]

type Key = String
type Value = User

data KeyValue = KeyValue !(Map.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

insertKey :: Key -> User -> Update KeyValue ()
insertKey k v = do
    KeyValue m <- get
    put (KeyValue (Map.insert k v m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey k = do
    KeyValue m <- ask
    return (Map.lookup k m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

text :: User -> Text
text = toLazyText . fromValue . toJSON

user:: Text -> Maybe User
user =  decode . encodeUtf8

read' :: AcidState KeyValue -> String -> IO Text
read' s' k = do
    u' <- query s' (LookupKey k)
    case u' of
        Just u ->return $ text u
        Nothing -> return $ text (User "" "" "" "")

write' :: AcidState KeyValue -> String -> Text -> IO ()
write' s' k v = do
    let u = fromMaybe (error "invalid json") (user v)
    update s' (InsertKey k u)

open' :: IO (AcidState KeyValue)
open' = openLocalState (KeyValue Map.empty)

close' :: AcidState KeyValue -> IO ()
close' = closeAcidState
