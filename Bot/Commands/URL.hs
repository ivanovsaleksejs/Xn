{-# LANGUAGE OverloadedStrings #-}
module Bot.Commands.URL where

import Data.List (tail, isPrefixOf)
import Data.Char (toLower)
import Data.ByteString.Lazy.Char8 (unpack)

import Control.Exception as E
import Control.Applicative ((<$>))
import Control.Monad.RWS hiding (join)

import System.Process
import System.IO

import Bot.Config (Net)

-- Filter urls from string
urls :: String -> [String]
urls = filter (\x -> "http://" `isPrefixOf` x || "https://" `isPrefixOf` x) . words

-- Fetch titles from all urls in string
getTitles :: String -> Net [String]
getTitles = liftIO . sequence . map fetchTitle . urls

-- If title cannot be fetched, return empty string
getTitle :: String -> IO String
getTitle url = fetchTitle url

-- Fetch a title from web page
fetchTitle url = do
    (_, Just hOut, _, hProc) <- createProcess (
                                      (shell (makewget url))
                                                                        { std_out = CreatePipe }
                                                                                                        )
    exitCode <- waitForProcess hProc
    output <- hGetContents hOut
    return output 

makewget url = "wget -qO- '" ++ url ++ "'  |   perl -l -0777 -ne 'print $1 if /<title.*?>\\s*(.*?)\\s*<\\/title/si' |   recode html.."
