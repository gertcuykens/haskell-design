{-# LANGUAGE OverloadedStrings #-}
module Login (FB.User,url,object,email,name,uid) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Facebook as FB
import Network.HTTP.Conduit (withManager)

app :: FB.Credentials
app = FB.Credentials "localhost" "249348058430770" ""

rrl :: FB.RedirectUrl
rrl = "http://localhost:9160/state.htm"

perms :: [FB.Permission]
perms = ["user_about_me", "email"]

url :: MonadIO m => m Text
url = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 rrl perms

object :: MonadIO m => FB.Argument -> m FB.User
object c = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ do
    let m = FB.Id $ pack "me"
    t <- FB.getUserAccessTokenStep2 rrl [c]
    FB.getUser m [] (Just t)

email :: FB.User -> Text
email u = fromMaybe "" (FB.userEmail u)

name :: FB.User -> Text
name u = fromMaybe "" (FB.userName u)

uid :: FB.User -> Text
uid u = FB.idCode $ FB.userId u

