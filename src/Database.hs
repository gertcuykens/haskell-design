{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell, DeriveGeneric#-}
module Database (AcidState, Table, read', write', open', close') where
import Blaze.ByteString.Builder.Internal.Types (Builder)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Blaze.ByteString.Builder (fromLazyByteString)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens ((?=), at, from, makeIso, view)
import Data.Maybe (fromMaybe)
import Data.Aeson ((.:), (.=), Value(Object), FromJSON(parseJSON), ToJSON(toJSON), object, decode, encode, json, fromJSON, Result)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text, unpack)
import Data.Acid (AcidState, Update, Query, openLocalState, makeAcidic)
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
import System.IO.Streams.Attoparsec (parseFromStream)

data User = User {city::Text
                 ,country::Text
                 ,phone::Text
                 ,email::Text} deriving (Show, Typeable)
$(deriveSafeCopy 0 'base ''User)
$(deriveJSON P.id ''User)

newtype Table = Table (Map.Map String User) deriving (Show, Typeable)
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
open' = openLocalState (Table Map.empty)

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

main :: IO (Result Feed)
main = withConnection (openConnection "earthquake.usgs.gov" 80) $ \c -> do
    q <- Client.buildRequest $ do
        Client.http Client.GET "/earthquakes/feed/v1.0/summary/all_day.geojson"
        Client.setAccept "application/json"
    Client.sendRequest c q Client.emptyBody
    Client.receiveResponse c (\_ i -> parseJSONFromStream i :: IO (Result Feed))

parseJSONFromStream :: FromJSON a => Streams.InputStream BS.ByteString -> IO (Result a)
parseJSONFromStream = parseFromStream $ fmap fromJSON json

createJSONtoStream :: Streams.OutputStream Builder -> IO ()
createJSONtoStream = Streams.write (Just (fromLazyByteString (encode (User "" "" "" ""))))
--createJSONtoStream = Streams.write (Just (fromString "Hello World\n"))
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

