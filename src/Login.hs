{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module Login (
    fbUrl,
    fbEmail,
    fbName,
    fbTest
) where

import qualified Facebook as FB
import Network.HTTP.Conduit (withManager)
import Data.Text
import Data.ByteString.Internal (ByteString)
import Control.Exception
import qualified Data.ByteString.Char8 as C

app :: FB.Credentials
app = FB.Credentials "localhost" "249348058430770" "..."

url :: FB.RedirectUrl
url = "http://localhost/state.htm"

perms :: [FB.Permission]
perms = ["user_about_me", "email"]

--fbUrl :: Monad m => FB.FacebookT FB.Auth m Text
fbUrl :: IO Text
fbUrl = withManager $ \manager -> FB.runFacebookT app manager $ FB.getUserAccessTokenStep1 url perms

--fbEmail :: Monad m => (ByteString, ByteString) -> FB.FacebookT FB.Auth m (Maybe Text)
fbEmail :: FB.Argument -> IO (Maybe Text)
fbEmail c = withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 url [c]
    u <- FB.getUser "me" [] (Just t)
    return $ FB.userEmail u

fbName :: FB.Argument -> IO (Maybe Text)
fbName c = withManager $ \manager -> FB.runFacebookT app manager $ do
    t <- FB.getUserAccessTokenStep2 url [c]
    u <- FB.getUser "me" [] (Just t)
    return $ FB.userName u

fbTest :: IO ()
fbTest = do
    u <- fbUrl
    print u

    --a <- readLn
    --e <- try . fbEmail $ a

    let a = ("code","test")
    e <- try . fbEmail $ (\(x,y) -> (C.pack x, C.pack y)) a
    case e of
        Left e -> print $ "error: " ++ show (e :: SomeException)
        Right Nothing -> print "doh!"
        Right (Just e) -> print e
