{-# LANGUAGE OverloadedStrings #-}
module Keys where
import Network.OAuth.OAuth2 (OAuth2(..))

googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = ""
                   , oauthClientSecret = ""
                   --, oauthCallback = Just "https://developers.google.com/oauthplayground"
                   , oauthCallback = Just "https://localhost:9160/code.htm"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
                   , oauthAccessToken = Nothing}

facebookKey :: OAuth2
facebookKey = OAuth2 { oauthClientId = ""
                     , oauthClientSecret = ""
                     , oauthCallback = Just "https://localhost:9160/code.htm"
                     , oauthOAuthorizeEndpoint = "https://www.facebook.com/dialog/oauth"
                     , oauthAccessTokenEndpoint = "https://graph.facebook.com/oauth/access_token"
                     , oauthAccessToken = Nothing}
