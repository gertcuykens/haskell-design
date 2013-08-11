{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module GoogleEmail where
import Data.Aeson (FromJSON)
import Data.Aeson.TH (deriveJSON)
import Data.ByteString.Char8 ( ByteString, pack, putStrLn )
import Data.Text (Text)
import Keys (googleKey)
import Network.OAuth.OAuth2 (AccessToken, QueryParams, OAuth2Result, appendQueryParam, fetchAccessToken, fetchRefreshToken, authGetJSON, authorizationUrl, refreshToken )
import Prelude hiding (id, putStrLn)
import qualified Prelude as P (id)

data Email = Email { id             :: Text
                   , email          :: Text
                   , verified_email :: Bool
                   } deriving (Show)

$(deriveJSON P.id ''Email)

query :: QueryParams
query = [("scope", "https://www.googleapis.com/auth/userinfo.email")
        ,("access_type", "offline")
        ,("approval_prompt", "force")]

url :: QueryParams -> ByteString
url state = authorizationUrl googleKey `appendQueryParam` query `appendQueryParam` state

tid :: ByteString -> IO (OAuth2Result AccessToken)
tid = fetchAccessToken googleKey

uid :: FromJSON a => AccessToken -> IO (OAuth2Result a)
uid t = authGetJSON t "https://www.googleapis.com/oauth2/v2/userinfo"

f1 :: String -> [Int] -> [Int]
f1 s i = do
    let a = read . takeWhile (/=',') $ s :: Int
    let b = drop 1 . dropWhile (/=',') $ s
    if b /= [] then f1 b (i++[a]) else i++[a]

f2 :: String -> (ByteString,[Int])
f2 s = do
    let a = drop 1 . takeWhile (/='&') . dropWhile (/='=') $ s
    let c = drop 6 . dropWhile (/='&') $ s
    (pack c, f1 a [])

test :: IO ()
test = do
    putStrLn $ url [("state", "0,1,2,3")]
    (code,state) <- fmap f2 getLine
    (Right t) <- tid code
    f t state
    case refreshToken t of
        Nothing -> putStrLn "Failed to fetch refresh token"
        Just rt -> do
            (Right r) <- fetchRefreshToken googleKey rt
            f r state
    where f t s = (uid t :: IO (OAuth2Result Email)) >>= \(Right x) -> print (x,s)

-- validateToken :: FromJSON a => AccessToken -> IO (OAuth2Result a)
-- validateToken token = authGetJSON token "https://www.googleapis.com/oauth2/v1/tokeninfo"

-- data Token = Token { issued_to      :: Text
--                    , audience       :: Text
--                    , user_id        :: Text
--                    , scope          :: Text
--                    , expires_in     :: Integer
--                    , email          :: Text
--                    , verified_email :: Bool
--                    , access_type    :: Text
--                    } deriving (Show)
--
-- $(deriveJSON P.id ''Token)

