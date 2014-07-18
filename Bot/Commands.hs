{-# LANGUAGE OverloadedStrings #-}
module Bot.Commands

where

import Data.List
import Data.Monoid (mconcat)
import Data.Text (unpack)

import Control.Exception as E
import Control.Monad.Reader as R

import Network.HTTP.Conduit (simpleHttp, HttpException)

import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (attributeIs, content, element,
                         fromDocument, ($//), (&//), (>=>))
import System.Time
import System.Random

import Bot.Config

--
-- Filter urls from string and apply a function
--
urls :: (([Char] -> Bool) -> [String] -> t) -> String -> t
urls f s = f (\x -> "http://" `isPrefixOf` x || "https://" `isPrefixOf` x) (words s)

--
-- Fetch title from first url in string
-- TODO: fetch titles from all urls in string
--
getTitles :: String -> Net String
getTitles = io . head . map getTitle . urls filter

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
    return . unpack . mconcat $ cursor $// element "title" &// content

--
-- Generate a random Integer in range 0..n
--
rand :: String -> Net String
rand s
    | not  (isInteger s) || n == 0 = return "y u do dis"
    | otherwise                    = io $ fmap (show . flip mod n) randomIO
    where
        n           = read s :: Int
        isInteger s = case reads s :: [(Integer, String)] of
            [(_, "")] -> True
            _         -> False 

--
-- Calculate and pretty print the uptime
--
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = R.join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    where
        secs    = abs $ tdSec td  ; mins   = secs   `div` 60
        hours   = mins   `div` 60 ; days   = hours  `div` 24
        months  = days   `div` 28 ; years  = months `div` 12
        f (i,s) | i == 0    = []
                | otherwise = show i ++ s
