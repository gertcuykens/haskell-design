{-# LANGUAGE OverloadedStrings #-}
module Login (FB.User(..),url,usr,email,name,uid) where

import Network.HTTP.Conduit (withManager)
import Data.Text (Text, pack, unpack)
import Control.Monad.IO.Class (MonadIO, liftIO)
--import Data.ByteString.Char8 (unpack)
import qualified Facebook as FB

app :: FB.Credentials
app = FB.Credentials "localhost" "249348058430770" "..."

rrl :: FB.RedirectUrl
rrl = "http://localhost:8000/state.htm"

perms :: [FB.Permission]
perms = ["user_about_me", "email"]

url :: MonadIO m => m Text
url = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 rrl perms

usr :: MonadIO m => FB.Argument -> m FB.User
usr c = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ do
    let m = FB.Id $ pack "me"
    t <- FB.getUserAccessTokenStep2 rrl [c]
    FB.getUser m [] (Just t)

email :: FB.User -> Text
email u = case FB.userEmail u of
               Just e -> e
               Nothing -> ""

name :: FB.User -> Text
name u = case FB.userName u of
              Just n -> n
              Nothing -> ""

uid :: FB.User -> String
uid u = unpack $ FB.idCode $ FB.userId u

