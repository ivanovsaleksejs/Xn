module Bot.General

where

import Data.List

import Text.Printf

import Control.Monad.RWS

import Bot.Config

s' = isPrefixOf "s/"

--
-- Send a privmsg to the channel/user + server
--
privmsg :: String -> String -> Net ()
privmsg target s = write "PRIVMSG" (target ++ " :" ++ s)

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

--
-- Clear message of prefix
--
clean     = drop 1 . dropWhile (/= ':') . drop 1
