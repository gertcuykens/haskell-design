{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell #-}
module Google (User(..), token, userinfo') where

import Control.Applicative ((<$>), (<*>))
--import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value(Object), parseJSON, (.:), (.:?))
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
--import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Keys (googleKey)
--import Network.HTTP.Conduit (Response)
import Network.OAuth.OAuth2.HttpClient
import Network.OAuth.OAuth2
import Prelude hiding (id)
import qualified Prelude as P (id)
--import System.Environment (getArgs)

data Token = Token { issued_to   :: Text
                   , audience    :: Text
                   , user_id     :: Maybe Text
                   , scope       :: Text
                   , expires_in  :: Integer
                   , email       :: Maybe Text
                   , verified_email :: Maybe Bool
                   , access_type :: Text
                   } deriving (Show)

instance FromJSON Token where
    parseJSON (Object o) = Token
                           <$> o .:  "issued_to"
                           <*> o .:  "audience"
                           <*> o .:? "user_id"
                           <*> o .:  "scope"
                           <*> o .:  "expires_in"
                           <*> o .:? "email"
                           <*> o .:? "verified_email"
                           <*> o .:  "access_type"
    parseJSON _ = mzero

data User = User { id          :: Text
                 , name        :: Text
                 , given_name  :: Text
                 , family_name :: Text
                 , link        :: Text
                 , picture     :: Text
                 , gender      :: Text
                 , birthday    :: Text
                 , locale      :: Text
                 } deriving (Show)

$(deriveJSON P.id ''User)

main :: IO ()
main = do
    print $ authorizationUrl googleKey `appendQueryParam'` googleScopeUserInfo `appendQueryParam'` googleAccessOffline
    putStrLn "visit the url and paste code here: "
    code <- fmap BS.pack getLine
    (Just (AccessToken accessToken refreshToken)) <- requestAccessToken googleKey code
    print (accessToken, refreshToken)
    validateToken accessToken >>= print
    (validateToken' accessToken :: IO (Maybe Token)) >>= print
    case refreshToken of
        Nothing -> print "Failed to fetch refresh token"
        Just tk -> do
            (Just (AccessToken accessToken refreshToken)) <- refreshAccessToken googleKey tk
            print (accessToken, refreshToken)
            validateToken accessToken >>= print
            (validateToken' accessToken :: IO (Maybe Token)) >>= print

googleScopeEmail :: QueryParams
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

googleScopeUserInfo :: QueryParams
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

googleAccessOffline :: QueryParams
googleAccessOffline = [("access_type", "offline")
                      ,("approval_prompt", "force")]

validateToken :: BS.ByteString -> IO BL.ByteString
validateToken accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo" `appendQueryParam` (accessTokenToParam accessToken))

validateToken' :: FromJSON Token => BS.ByteString -> IO (Maybe Token)
validateToken' accessToken = doJSONGetRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo" `appendQueryParam` (accessTokenToParam accessToken))

userinfo :: BS.ByteString -> IO BL.ByteString
userinfo accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v2/userinfo" `appendQueryParam` (accessTokenToParam accessToken))

userinfo' :: FromJSON User => BS.ByteString -> IO (Maybe User)
userinfo' accessToken = doJSONGetRequest ("https://www.googleapis.com/oauth2/v2/userinfo" `appendQueryParam` (accessTokenToParam accessToken))

token :: FromJSON User => BS.ByteString -> IO BS.ByteString
token code = do
    (Just (AccessToken accessToken refreshToken)) <- requestAccessToken googleKey code
    return accessToken

