{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Json (JsState(..),jsread,jswrite,jsopen,jsclose) where

import Control.Applicative
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)
import Data.Maybe
import Data.Aeson hiding (Value)
import Data.Aeson.Encode (fromValue)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (decodeUtf8,encodeUtf8)

import Data.Acid
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import System.Environment
import System.IO
import System.Exit
import Data.SafeCopy
import Data.Typeable

import qualified Data.Map as MP

data User = User { city :: Text
                 , country :: Text
                 , phone :: Text
                 , email :: Text}
    deriving (Typeable)

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

data KeyValue = KeyValue !(MP.Map Key Value)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

type JsState = AcidState KeyValue

insertKey :: Key -> User -> Update KeyValue ()
insertKey k v = do
    KeyValue m <- get
    put (KeyValue (MP.insert k v m))

lookupKey :: Key -> Query KeyValue (Maybe Value)
lookupKey k = do
    KeyValue m <- ask
    return (MP.lookup k m)

$(makeAcidic ''KeyValue ['insertKey, 'lookupKey])

text :: User -> Text
text = toLazyText . fromValue . toJSON

user:: Text -> Maybe User
user =  decode . encodeUtf8

jsread :: AcidState KeyValue -> String -> IO Text
jsread s k = do
    u <- query s (LookupKey k)
    case u of
        Just u -> return (text u)
        _ -> return (text (User "" "" "" ""))

jswrite :: AcidState KeyValue -> String -> Text -> IO ()
jswrite s k v = do
    let u = fromMaybe (error "invalid json") (user v)
    update s (InsertKey k u)

jsopen :: IO (AcidState KeyValue)
jsopen = openLocalState (KeyValue MP.empty)

jsclose :: AcidState KeyValue -> IO()
jsclose = closeAcidState
