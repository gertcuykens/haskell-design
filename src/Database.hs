{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell, DeriveGeneric#-}
module Database (AcidState, Table, read', write', open', close') where
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens ((?=), at, from, makeIso, view)
import Data.Maybe (fromMaybe)
import Data.Aeson ((.:), (.=), Value(Object), FromJSON(parseJSON), ToJSON(toJSON), object, decode, encode)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text, unpack)
import Data.Acid (AcidState, Update, Query, openLocalStateFrom, makeAcidic)
import Data.Acid.Advanced (query', update')
import Data.Acid.Local (createCheckpointAndClose, createArchive)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable (Typeable)
import qualified Data.Map as Map (Map, empty)
import GHC.Generics (Generic)
import Network.Http.Client as Client
import Prelude hiding (id)
import qualified Prelude as P (id)
import qualified System.IO.Streams as Streams

data User = User {city::Text
                 ,country::Text
                 ,phone::Text
                 ,email::Text} deriving Typeable
$(deriveSafeCopy 0 'base ''User)
$(deriveJSON P.id ''User)

newtype Table = Table (Map.Map String User)
$(deriveSafeCopy 0 'base ''Table)
--table :: Simple Iso (Map.Map Key User) Table
--table = iso Table get Table
$(makeIso ''Table)
insertKey :: String -> User -> Update Table ()
insertKey k v = from table.at k?=v
lookupKey :: String -> Query Table (Maybe User)
lookupKey k = view (from table.at k)
$(makeAcidic ''Table ['insertKey, 'lookupKey])

read' :: MonadIO m => AcidState Table -> Text -> m BL.ByteString
read' s' k' = do
    let k = unpack k'
    u' <- query' s' (LookupKey k)
    case u' of
        Just u -> return $ encode u
        Nothing -> return $ encode (User "" "" "" "")

write' :: MonadIO m => AcidState Table -> Text -> BL.ByteString -> m ()
write' s' k' v' = do
    let k = unpack k'
    let v = fromMaybe (error "invalid json") (decode v')
    update' s' (InsertKey k v)

open' :: IO (AcidState Table)
open' = openLocalStateFrom "data/Table" (Table Map.empty)

close' :: AcidState Table -> IO ()
close' s' = createCheckpointAndClose s' >> createArchive s'

{-
data MetaData = MetaData {
      url :: String,
      title :: String
} deriving (Show, Generic)
instance FromJSON MetaData

data Properties = Properties {
      detail :: String,
      mag :: Double
} deriving (Show, Generic)
instance FromJSON Properties

data Feature = Feature {
      id :: String,
      properties :: Properties
} deriving (Show, Generic)
instance FromJSON Feature

data Feed = Feed {
      metadata :: MetaData,
      features :: [Feature]
} deriving (Show, Generic)
instance FromJSON Feed

main :: IO ()
main = do
  c <- Client.openConnection "earthquake.usgs.gov" 80
  q <- Client.buildRequest $ do
         Client.http Client.GET "/earthquakes/feed/v1.0/summary/all_day.geojson"
         Client.setAccept "application/json"
  Client.sendRequest c q Client.emptyBody
  x <- Client.receiveResponse c jsonHandler
  print x
  Client.closeConnection c

jsonHandler :: Response -> Streams.InputStream BS.ByteString -> IO (Maybe Feed)
jsonHandler _ i = do
  c <- Streams.toList i
  let f = decode (BL.fromChunks c) :: Maybe Feed
  return f
-}

{-
import qualified Data.Text.Lazy.Builder as TL (toLazyText)
import qualified Data.Text.Lazy.Internal as TL (Text)
import qualified Data.Text.Lazy.Encoding as TL (encodeUtf8)
import Data.Aeson.Encode (fromValue)

where f = decode . TL.encodeUtf8
where f = TL.toLazyText . fromValue . toJSON

import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)

insertKey :: Key -> User -> Update Table ()
insertKey k v = do
    Table m <- get
    put (Table (Map.insert k v m))

lookupKey :: Key -> Query Table (Maybe User)
lookupKey k = do
    Table m <- ask
    return (Map.lookup k m)

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

