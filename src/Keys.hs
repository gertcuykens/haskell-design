{-# LANGUAGE OverloadedStrings #-}
module Keys where
import Network.OAuth.OAuth2 (OAuth2(..))

googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = ""
                   , oauthClientSecret = ""
                   , oauthCallback = Just "http://localhost:9160"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"}

facebookKey :: OAuth2
facebookKey = OAuth2 { oauthClientId = ""
                     , oauthClientSecret = ""
                     , oauthCallback = Just "http://localhost:9160"
                     , oauthOAuthorizeEndpoint = "https://www.facebook.com/dialog/oauth"
                     , oauthAccessTokenEndpoint = "https://graph.facebook.com/oauth/access_token"}

