{-# LANGUAGE OverloadedStrings #-}
module Bot.Commands.URL where

import Data.List (tail, isPrefixOf)
import Data.Char (toLower)
import Data.ByteString.Lazy.Char8 (unpack)

import Control.Exception as E
import Control.Applicative ((<$>))
import Control.Monad.RWS hiding (join)

import Network.HTTP.Types.Header (hRange, renderByteRange, ByteRange(ByteRangeFromTo))
import Network.HTTP.Conduit (newManager, httpLbs, HttpException, requestHeaders,
                                conduitManagerSettings, parseUrl, responseBody)

import Bot.Config (Net)

-- Filter urls from string
urls :: String -> [String]
urls = filter (\x -> "http://" `isPrefixOf` x || "https://" `isPrefixOf` x) . words

-- Fetch titles from all urls in string
getTitles :: String -> Net [String]
getTitles = liftIO . sequence . map getTitle . urls

-- If title cannot be fetched, return empty string
getTitle :: String -> IO String
getTitle url = E.catch (fetchTitle url) (\e -> const(return "") (e :: HttpException ))

-- Fetch a title from web page
fetchTitle url = methodHead url >>= return . findTitle . take 2000 . unpack
    where
        methodHead s = do 
            m <- newManager conduitManagerSettings
            r <- parseUrl s
            let r' = r {
                requestHeaders = [(hRange, renderByteRange $ ByteRangeFromTo 20 2000)]
            }
            responseBody <$> httpLbs r' m
        findTitle a
            | a == []      = ""
            | checkTitle a = title a 
            | otherwise    = findTitle $ tail a
        title      = takeWhile (/= '<') . take 150 . drop 1 . dropWhile (/= '>')
        checkTitle = ("<title" ==) . map toLower . take 6 
