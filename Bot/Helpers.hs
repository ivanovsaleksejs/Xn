module Bot.Helpers

where

import Data.List

import Control.Monad

import Bot.Config
import Bot.General
import Bot.Commands.History
import Bot.Commands.URL

--
-- Helpers
--

[ping, h', lb, cl] = map isPrefixOf ["PING :", "!history", ':' : lambdabot, (':' : clojurebot)]

priv      = ("PRIVMSG" ==) . (!! 1) . words
hasUrls   = liftM2 (&&) isChan (urls any . clean)

evlb s    = (isPrefixOf "> " $ clean s) && sender s /= "daGrevis" -- Yes, this actually is hardcode for daGrevis

[tolb, tocl] = map (\x -> flip any x . flip isPrefixOf . clean) [lbCmd, [",("]]

isChan s  = length w > 2 && w !! 2 == chan
    where w = words s

pong x    = write "PONG" (':' : drop 6 x)
resp x    = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)

showTitles s = getTitles c >>= mapM_ (sendDelayed . privmsg t)
    where
        (t, c) = (target s, clean s)

hist :: String -> MessageStack -> Net ()
hist s stack = hist' (sender s) (reverse (take num stack))
    where
        num
            | isInteger c = n
            | otherwise   = 50
        c           = drop 9 $ clean s
        n           = read c :: Int

hist'     = mapM_ . sendHistory
addSender = ('<' :) . (++ ">")
