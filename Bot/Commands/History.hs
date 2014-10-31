module Bot.Commands.History

where

import Control.Concurrent
import Control.Monad.RWS

import System.Random

import Bot.Config
import Bot.General

--
-- Send a history entry to user
--
sendHistory :: String -> Msg -> Net ()
sendHistory x (time, s) = sendDelayed . join . io $ fmap (write "PRIVMSG ") msg
    where
        (t, c) = (sender s, clean s)
        msg    = fmap (\t' -> concat [x, " :", t', " <", t, "> ", c]) time

--
-- Sends multiple messages with random delay
--
sendDelayed :: Net a -> Net ()
sendDelayed = (>> (io $ threadDelay =<< fmap interval randomIO))
    where
        interval = (*10^5) . (+) 5 . flip mod 10

--
-- Get sender's last message that is not s/ command
--
lastmsg :: String -> MessageStack -> String
lastmsg a stack
    | length f == 0 = ""
    | otherwise     = snd $ head f
    where
        f  = filter (\s -> a == (sender $ snd s) && not (s' $ clean $ snd s)) stack

--
-- Set target of response
--
target :: String -> String
target x  = if parts !! 1 == "PRIVMSG" && t /= lambdabot && t /= clojurebot && ch /= chan then t else chan
    where
        parts  = words x
        ch     = parts !! 2
        t      = takeWhile (/= '!') $ drop 1 $ parts !! 0

--
-- Get sender of message
--
sender :: String -> String
sender x
    | x == ""                 = chan
    | parts !! 1 == "PRIVMSG" = t
    | otherwise               = chan
    where
        parts  = words x
        t      = takeWhile (/= '!') $ drop 1 $ parts !! 0
