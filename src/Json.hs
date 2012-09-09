{-# LANGUAGE OverloadedStrings #-}
module Json (UserState,readS,writeS) where

import Control.Applicative
import Control.Concurrent (MVar, modifyMVar_, readMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)
import Data.Maybe
import Data.Aeson
import Data.Aeson.Encode (fromValue)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Internal (Text)
import Data.Text.Lazy.Encoding (decodeUtf8,encodeUtf8)

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

text :: User -> Text
text = toLazyText . fromValue . toJSON

user:: Text -> Maybe User
user =  decode . encodeUtf8

delete :: Eq a => a -> [(a,b)] -> [(a,b)]
delete i xs = [x|x<-xs,(fst x)/= i]

type UserState = [(String,User)]

readS :: String -> MVar UserState -> IO Text
readS i s = do
    xs <- readMVar s
    let e = lookup i xs
    case e of
        Just e -> return (text e)
        _ -> return (text (User "" "" "" ""))

writeS :: String -> Text -> MVar UserState -> IO ()
writeS i m s = modifyMVar_ s $ \xs -> do
    let u = fromMaybe (error "invalid user json") (user m)
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

