{-# LANGUAGE OverloadedStrings #-}
module Keys where
import Network.OAuth.OAuth2 (OAuth2(..))

googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "522156758812-09f5qv0e4gqjdjqfocerqcud5m5jutau.apps.googleusercontent.com"
                   , oauthClientSecret = ""
                   --, oauthCallback = Just "https://developers.google.com/oauthplayground"
                   , oauthCallback = Just "http://localhost:9160"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                   , oauthAccessToken = Nothing}

facebookKey :: OAuth2
facebookKey = OAuth2 { oauthClientId = ""
                     , oauthClientSecret = ""
                     , oauthCallback = Just "http://localhost:9160"
                     , oauthOAuthorizeEndpoint = "https://www.facebook.com/dialog/oauth"
                     , oauthAccessTokenEndpoint = "https://graph.facebook.com/oauth/access_token"
                     , oauthAccessToken = Nothing}
