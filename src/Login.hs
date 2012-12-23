{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Login (AccessToken(..), authToken, userinfo) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Internal as BL
import Network.HTTP.Conduit (Response)
import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2
import System.Environment

githubKeys :: OAuth2
githubKeys = OAuth2 { oauthClientId = "xxxxxxxxxxxxxxx"
                    , oauthClientSecret = "xxxxxxxxxxxxxxxxxxxxxx"
                    , oauthCallback = Just "http://localhost:9160/code.htm"
                    , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                    , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                    , oauthAccessToken = Nothing}

githubScope :: QueryParams
githubScope = [("scope", ""), ("access_type", "offline")]

facebookKeys :: OAuth2
facebookKeys = OAuth2 { oauthClientId = ""
                      , oauthClientSecret = ""
                      , oauthCallback = Just "http://localhost:9160/code.htm"
                      , oauthOAuthorizeEndpoint = "https://www.facebook.com/dialog/oauth"
                      , oauthAccessTokenEndpoint = "https://graph.facebook.com/oauth/access_token"
                      , oauthAccessToken = Nothing}

facebookScope :: QueryParams
facebookScope = [("scope", "user_about_me,email")]

googleKeys :: OAuth2
googleKeys = OAuth2 { oauthClientId = ""
                    , oauthClientSecret = ""
                    , oauthCallback = Just "http://localhost:9160/code.htm"
                    , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                    , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                    , oauthAccessToken = Nothing}

googleScope :: QueryParams
googleScope = [("scope", "https://www.googleapis.com/auth/userinfo.profile"), ("access_type", "offline")]

main :: IO ()
main = do
    (x:_) <- getArgs
    case x of
        "normal" -> normalCase
        "offline" -> offlineCase

normalCase :: IO ()
normalCase = do
    print $ (authorizationUrl googleKeys) `appendQueryParam'` googleScope
    putStrLn "visit the url and paste code here: "
    code <- getLine
    (Just (AccessToken accessToken refreshToken)) <- authToken (BS.pack code)
    --validate accessToken >>= print
    userinfo accessToken >>= print

offlineCase :: IO ()
offlineCase = do
    print $ (authorizationUrl googleKeys) `appendQueryParam'` googleScope
    putStrLn "visit the url and paste code here: "
    code <- getLine
    (Just (AccessToken accessToken refreshToken)) <- authToken (BS.pack code)
    case refreshToken of
        Nothing -> print "Failed to fetch refresh token"
        Just t -> do
        (Just (AccessToken accessToken Nothing)) <- authToken t
        --validate accessToken >>= print
        userinfo accessToken >>= print

authToken :: BS.ByteString -> IO (Maybe AccessToken)
authToken = requestAccessToken googleKeys

validate :: BS.ByteString -> IO (Response BL.ByteString)
validate accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo" `appendQueryParam` (accessTokenToParam accessToken))

userinfo :: BS.ByteString -> IO (Response BL.ByteString)
userinfo accessToken = doSimpleGetRequest ("https://www.googleapis.com/oauth2/v2/userinfo" `appendQueryParam` (accessTokenToParam accessToken))

-- obtain a new access token with refresh token, which turns out only in response at first time.
-- Revoke Access https://www.google.com/settings/security
--
--rrl :: FB.RedirectUrl
--rrl = "http://localhost:9160/state.htm"
--url :: MonadIO m => m Text
--url = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 rrl extraParams
--      where extraParams = ["user_about_me", "email"]
--url >>= print

{-
facebookScope :: SimpleQuery
facebookScope = [("scope", "user_about_me,email")]

googleScopeEmail :: SimpleQuery
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

googleScopeUserInfo :: SimpleQuery
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]
-}

{-
import qualified Facebook as FB

app :: FB.Credentials
app = FB.Credentials "localhost" (pack (BS.unpack (oauthClientId facebookKeys))) (pack (BS.unpack(oauthClientSecret facebookKeys)))

object :: MonadIO m => FB.Argument -> FB.Id-> m FB.User
object c u = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 "http://localhost:9160/state.htm" [c]
    FB.getUser u [] (Just t)

email :: FB.User -> Text
email o = fromMaybe "" (FB.userEmail o)

name :: FB.User -> Text
name o = fromMaybe "" (FB.userName o)

uid :: FB.User -> Text
uid o = FB.idCode $ FB.userId o
-}

{-
instance FromJSON User where
    parseJSON (Object v) = User <$> v .: "id"
                                <*> v .: "name"
                                <*> v .: "given_name"
                                <*> v .: "family_name"
                                <*> v .: "link"
                                <*> v .: "picture"
                                <*> v .: "gender"
                                <*> v .: "birthday"
                                <*> v .: "locale"

instance ToJSON User where
    toJSON (User a b c d e f g h i) = object ["id"          .= a
                                             ,"name"        .= b
                                             ,"given_name"  .= c
                                             ,"family_name" .= d
                                             ,"link"        .= e
                                             ,"picture"     .= f
                                             ,"gender"      .= g
                                             ,"birthday"    .= h
                                             ,"locale"      .= i]
-}

{-
 "id": "116469479527388802962",
 "name": "Gert Cuykens",
 "given_name": "Gert",
 "family_name": "Cuykens",
 "link": "https://plus.google.com/116469479527388802962",
 "picture": "https://lh5.googleusercontent.com/-Ar9QIaaTSJA/AAAAAAAAAAI/AAAAAAAAAXY/9P7CBht8ZRw/photo.jpg",
 "gender": "male",
 "birthday": "0000-10-01",
 "locale": "en"
-}

