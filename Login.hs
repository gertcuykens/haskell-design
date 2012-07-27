{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Login (
    fbUrl,
    fbEmail
) where

import qualified Facebook as FB
import Network.HTTP.Conduit (withManager)
--import Data.Text
--import Data.ByteString.Internal (ByteString)

app :: FB.Credentials
app = FB.Credentials "localhost" "249348058430770" "..."

url :: FB.RedirectUrl
url = "http://localhost/fb"

perms :: [FB.Permission]
perms = ["user_about_me", "email"]

--fbUrl :: Monad m => FB.FacebookT FB.Auth m Text
fbUrl = withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 url perms

--fbEmail :: Monad m => (ByteString, ByteString) -> FB.FacebookT FB.Auth m (Maybe Text)
fbEmail c = withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 url [c]
    u <- FB.getUser "me" [] (Just t)
    return $ FB.userEmail u
