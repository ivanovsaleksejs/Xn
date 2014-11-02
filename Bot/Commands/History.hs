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
sendHistory target (time, s) = sendDelayed $ write "PRIVMSG " msg
    where
        (origin, body) = (sender s, clean s)
        msg            = concat [target, " :", time, " <", origin, "> ", body]

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
