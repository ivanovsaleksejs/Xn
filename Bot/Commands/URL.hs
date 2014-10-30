{-# LANGUAGE OverloadedStrings #-}
module Bot.Commands.URL

where

import Data.List
import Data.Monoid (mconcat)
import Data.String.Utils (join)
import Data.Text (unpack)

import Control.Exception as E
import Control.Monad.RWS hiding (join)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (attributeIs, content, element,
                         fromDocument, ($//), (&//), (>=>))

import Network.HTTP.Conduit (simpleHttp, HttpException)

import Bot.Config

--
-- Filter urls from string and apply a function
--
urls :: ((String -> Bool) -> [String] -> t) -> String -> t
urls f = f (\x -> "http://" `isPrefixOf` x || "https://" `isPrefixOf` x) . words

--
-- Fetch titles from all urls in string
--
getTitles :: String -> Net [String]
getTitles = io . sequence . map getTitle . urls filter

--
-- If title cannot be fetched, return empty string
--
getTitle :: String -> IO String
getTitle url = E.catch (fetchTitle url) (\e -> const(return "") (e :: HttpException ))

--
-- Fetch a title from web page
--
fetchTitle :: MonadIO m => String -> m String
fetchTitle url = do
    lbs <- simpleHttp url
    let doc = parseLBS lbs
        cursor = fromDocument doc
    return . take 150 . unpack . mconcat $ cursor $// element "title" &// content
