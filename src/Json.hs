{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Json (KeyValue(..),readS,writeS) where

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

readS :: AcidState KeyValue -> String -> IO Text
readS s k = do
    u <- query s (LookupKey k)
    case u of
        Just u -> return (text u)
        _ -> return (text (User "" "" "" ""))

user:: Text -> Maybe User
user =  decode . encodeUtf8

writeS :: AcidState KeyValue -> String -> Text -> IO ()
writeS s k v = do
    let u = fromMaybe (error "invalid user json") (user v)
    update s (InsertKey k u)

{-
delete :: Eq a => a -> [(a,b)] -> [(a,b)]
delete k xs = [x|x<-xs,(fst x)/= k]

readS :: MVar [(String,User)] -> String -> IO Text
readS s k = do
    xs <- readMVar s
    let u = lookup k xs
    case u of
        Just u -> return (text u)
        _ -> return (text (User "" "" "" ""))

writeS :: MVar [(String,User)] -> String -> Text -> IO ()
writeS s k v = modifyMVar_ s $ \xs -> do
    let v' = fromMaybe (error "invalid user json") (user v)
    let xs' = delete k xs
    return ((k,v'):xs')
-}

--import Data.Attoparsec.Text.Lazy
--user msg = User "" "" "" ""
--user msg = do
--    let v = parse json (encodeUtf8 msg)
--    case fromJSON v of
--         Success a -> a
--         Error s -> error s

--test::Text
--test="{\"city\":\"test\",\"country\":\"test\",\"phone\":\"test\",\"email\":\"test\"}"
