module Bot.Commands.URL where

import Data.List (isPrefixOf)

import Control.Monad.RWS hiding (join)

import System.Process
import System.IO

import Bot.Config.Basic
import Bot.Config.State
import Bot.Config.StateMethods

-- Filter urls from string
urls :: String -> [String]
urls = filter (\x -> "http://" `isPrefixOf` x || "https://" `isPrefixOf` x) . words

-- Fetch titles from all urls in string
getTitles :: String -> Net [String]
getTitles = liftIO . sequence . map getTitle . map (filter (/= '\'')) . urls

-- If title cannot be fetched, return empty string
getTitle :: String -> IO String
getTitle url = do
    (_, Just hOut, _, hProc) <- createProcess (shell $ makewget url) { std_out = CreatePipe }
    exitCode <- waitForProcess hProc
    output <- hGetContents hOut
    return output 

makewget url = "wget -qO- -T 30 '" ++ url ++ "' --user-agent='User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0' | tr -d '\n' | perl -l -0777 -ne 'print $1 if /<title.*?>\\s*(.*?)\\s*<\\/title/si' | recode html.. | head -c 150"
