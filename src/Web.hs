{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web (webServer) where

--import Control.Applicative ((<$>), optional)
--import Data.Maybe (fromMaybe)
--import Data.Text (Text)
--import Data.Text.Lazy (unpack)
import Happstack.Lite
--import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
--import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
--import qualified Text.Blaze.Html5 as H
--import qualified Text.Blaze.Html5.Attributes as A

fileServing :: ServerPart Response
fileServing = serveDirectory EnableBrowsing ["state.htm"] "../www"

webServer :: IO ()
webServer = serve Nothing fileServing
