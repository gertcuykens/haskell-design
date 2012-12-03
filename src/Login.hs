{-# LANGUAGE OverloadedStrings #-}
module Login (FB.Id(..),FB.User,url,object,email,name,uid) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Facebook as FB
import Network.HTTP.Conduit (withManager)

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Network.HTTP.Types (renderSimpleQuery, parseSimpleQuery)
import Network.HTTP.Types.URI (SimpleQuery)
import System.Environment

import Network.OAuth2.HTTP.HttpClient
import Network.OAuth2.OAuth2

githubKeys :: OAuth2
githubKeys = OAuth2 { oauthClientId = "xxxxxxxxxxxxxxx"
                    , oauthClientSecret = "xxxxxxxxxxxxxxxxxxxxxx"
                    , oauthCallback = Just "http://localhost:9160/state.htm"
                    , oauthOAuthorizeEndpoint = "http://developer.github.com/v3/oauth/"
                    , oauthAccessTokenEndpoint = "http://developer.github.com/v3/oauth/"
                    , oauthAccessToken = Nothing
                    }

googleKeys :: OAuth2
googleKeys = OAuth2 { oauthClientId = ""
                    , oauthClientSecret = ""
                    , oauthCallback = Just "http://localhost:9160/state.htm"
                    , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                    , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                    , oauthAccessToken = Nothing
                    }

facebookKeys :: OAuth2
facebookKeys = OAuth2 { oauthClientId = ""
                      , oauthClientSecret = ""
                      , oauthCallback = Just "http://localhost:9160/state.htm"
                      , oauthOAuthorizeEndpoint = "https://www.facebook.com/dialog/oauth"
                      , oauthAccessTokenEndpoint = "https://graph.facebook.com/oauth/access_token"
                      , oauthAccessToken = Nothing
                      }

facebookScope :: SimpleQuery
facebookScope = [("scope", "user_about_me,email")]

googleScopeEmail :: SimpleQuery
googleScopeEmail = [("scope", "https://www.googleapis.com/auth/userinfo.email")]

googleScopeUserInfo :: SimpleQuery
googleScopeUserInfo = [("scope", "https://www.googleapis.com/auth/userinfo.profile")]

url :: Text
url = pack $ BS.unpack $ authorizationUrl facebookKeys `BS.append` "&" `BS.append` extraParams
    where extraParams = renderSimpleQuery False facebookScope

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

main :: IO ()
main = do
    print $ unpack url
    (x:_) <- getArgs
    case x of
        "normal" -> normalCase
        "offline" -> offlineCase

normalCase :: IO ()
normalCase = do
          print $ authorizationUrl googleKeys `BS.append` "&" `BS.append` extraParams
          putStrLn "visit the url and paste code here: "
          code <- getLine
          (Just (AccessToken accessToken Nothing)) <- requestAccessToken googleKeys (BS.pack code)
          print accessToken
          validateToken accessToken >>= print
    where extraParams = renderSimpleQuery False googleScopeEmail

offlineCase :: IO ()
offlineCase = do
          print $ authorizationUrl googleKeys `BS.append` "&" `BS.append` extraParams
          putStrLn "visit the url and paste code here: "
          code <- getLine
          (Just (AccessToken accessToken refreshToken)) <- requestAccessToken googleKeys (BS.pack code)
          print (accessToken, refreshToken)
          validateToken accessToken >>= print
          case refreshToken of
            Nothing -> print "Failed to fetch refresh token"
            Just tk -> refreshAccessToken googleKeys tk >>= print
    where extraParams = renderSimpleQuery False $ ("access_type", "offline"):googleScopeEmail

validateToken accessToken = doSimplePostRequest ("https://www.googleapis.com/oauth2/v1/tokeninfo",(accessTokenToParam accessToken))

--
-- obtain a new access token with refresh token, which turns out only in response at first time.
-- Revoke Access https://www.google.com/settings/security
--
--rrl :: FB.RedirectUrl
--rrl = "http://localhost:9160/state.htm"
--url :: MonadIO m => m Text
--url = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 rrl extraParams
--      where extraParams = ["user_about_me", "email"]
--url >>= print

