{-# LANGUAGE OverloadedStrings #-}
module Login (UserId,url,email,name,uid) where

import qualified Facebook as FB
import Network.HTTP.Conduit (withManager)
import Control.Exception
import Data.Text

type UserId = FB.UserId

app :: FB.Credentials
app = FB.Credentials "localhost" "249348058430770" "..."

rrl :: FB.RedirectUrl
rrl = "http://localhost:8000/state.htm"

perms :: [FB.Permission]
perms = ["user_about_me", "email"]

url :: IO Text
url = withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 rrl perms

email :: FB.Argument -> IO (Maybe Text)
email c = withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 rrl [c]
    u <- FB.getUser "me" [] (Just t)
    return $ FB.userEmail u

name :: FB.Argument -> IO (Maybe Text)
name c = withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 rrl [c]
    u <- FB.getUser "me" [] (Just t)
    return $ FB.userName u

uid :: FB.Argument -> IO (FB.UserId)
uid c = withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 rrl [c]
    u <- FB.getUser "me" [] (Just t)
    return $ FB.userId u
