{-# LANGUAGE OverloadedStrings #-}
module Json (User,State,newS,readS,writeS) where

import Control.Applicative
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)

import Data.Maybe
import Data.Aeson
import Data.Aeson.Encode (fromValue)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (decodeUtf8,encodeUtf8)

import Login (UserId)

data User = User { city :: Text
                 , country :: Text
                 , phone :: Text
                 , email :: Text}

instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "city"
                                <*> v .: "country"
                                <*> v .: "phone"
                                <*> v .: "email"
    parseJSON _          = mzero

instance ToJSON User where
    toJSON (User a b c d)= object ["city" .= a
                                  ,"country" .= b
                                  ,"phone" .= c
                                  ,"email" .= d]

type State = MVar[(UserId,User)]

newS :: IO State
newS = newMVar []

jsonS :: User -> Text
jsonS = toLazyText . fromValue . toJSON

readS :: UserId -> State -> IO Text
readS i s = do
    xs <- readMVar s
    let e = lookup i xs
    case e of
        Just e -> return (jsonS e)
        _ -> return (jsonS (User "" "" "" ""))

user:: Text -> User
user msg = fromMaybe (error "invalid user json") (decode $ encodeUtf8 msg)

delete :: Eq a => a -> [(a,b)] -> [(a,b)]
delete i xs = [x|x<-xs,(fst x)/= i]

writeS :: UserId -> Text -> State -> IO ()
writeS i m s = modifyMVar_ s $ \xs -> do
    let u = user m
    let xs' = delete i xs
    return ((i,u):xs')

--import Data.Attoparsec.Text.Lazy
--user msg = User "" "" "" ""
--user msg = do
--    let v = parse json (encodeUtf8 msg)
--    case fromJSON v of
--         Success a -> a
--         Error s -> error s

--test::Text
--test="{\"city\":\"test\",\"country\":\"test\",\"phone\":\"test\",\"email\":\"test\"}"

{-
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving,
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}
import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Data            ( Data, Typeable )
import Happstack.Server     ( Response, ServerPart, dir, nullDir, nullConf, ok, simpleHTTP, toResponse )
import Data.Acid            ( AcidState, Query, Update, makeAcidic, openLocalState )
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )

data CounterState = CounterState { count :: Integer }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''CounterState)

initialCounterState :: CounterState
initialCounterState = CounterState 0

incCountBy :: Integer -> Update CounterState Integer
incCountBy n = do
    c@CounterState{..} <- get
    let newCount = count + n
    put $ c { count = newCount }
    return newCount

peekCount :: Query CounterState Integer
peekCount = count <$> ask

$(makeAcidic ''CounterState ['incCountBy, 'peekCount])

handlers :: AcidState CounterState -> ServerPart Response
handlers acid =
    msum [ dir "peek" $ do c <- query' acid PeekCount
                           ok $ toResponse $ "peeked at the count and saw: " ++ show c
         , do nullDir
              c <- update' acid (IncCountBy 1)
              ok $ toResponse $ "New count is: " ++ show c ]

update' :: (UpdateEvent event, MonadIO m) =>
           AcidState (EventState event) -- ^ handle to acid-state
        -> event                        -- ^ update event to execute
        -> m (EventResult event)

query'  :: (QueryEvent event , MonadIO m) =>
           AcidState (EventState event) -- ^ handle to acid-state
        -> event                        -- ^ query event to execute
        -> m (EventResult event)

data PeekCount  = PeekCount
data IncCountBy = IncCountBy Integer

dbServer :: IO ()
dbServer = do
    bracket (openLocalState initialCounterState)
    (createCheckpointAndClose)
    (\acid -> simpleHTTP nullConf (handlers acid))
-}
