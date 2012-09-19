{-# LANGUAGE OverloadedStrings #-}
module Login (User(..),url,usr,email,name,uid) where

import Network.HTTP.Conduit (withManager)
import Data.Text (Text)
import Data.ByteString.Char8 (unpack)
import qualified Facebook as FB

type User = FB.User

app :: FB.Credentials
app = FB.Credentials "localhost" "249348058430770" "***"

rrl :: FB.RedirectUrl
rrl = "http://localhost:8000/state.htm"

perms :: [FB.Permission]
perms = ["user_about_me", "email"]

url :: IO Text
url = withManager $ \manager -> FB.runFacebookT app manager $ do
    FB.getUserAccessTokenStep1 rrl perms

usr :: FB.Argument -> IO FB.User
usr c = withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 rrl [c]
    u <- FB.getUser "me" [] (Just t)
    return u

email :: FB.User -> Text
email u = case FB.userEmail u of
              Just e -> e

name :: FB.User -> Text
name u = case FB.userName u of
              Just n -> n

uid :: FB.User -> String
uid u = unpack $ FB.userId u
