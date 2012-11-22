{-# LANGUAGE OverloadedStrings #-}
module Login (FB.Id(..),FB.User,url,object,email,name,uid) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Facebook as FB
import Network.HTTP.Conduit (withManager)

app :: FB.Credentials
app = FB.Credentials "localhost" "249348058430770" "..."

rrl :: FB.RedirectUrl
rrl = "http://localhost:9160/state.htm"

perms :: [FB.Permission]
perms = ["user_about_me", "email"]

url :: MonadIO m => m Text
url = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 rrl perms

object :: MonadIO m => FB.Argument -> FB.Id-> m FB.User
object c u = liftIO $ withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 rrl [c]
    FB.getUser u [] (Just t)

email :: FB.User -> Text
email o = fromMaybe "" (FB.userEmail o)

name :: FB.User -> Text
name o = fromMaybe "" (FB.userName o)

uid :: FB.User -> Text
uid o = FB.idCode $ FB.userId o

