module Bot.Helpers

where

import Data.List

import Bot.Config
import Bot.Commands.History

--
-- Helpers
--
helper s stack fs = head $
    [f s stack | (c, f) <- fst fs, c s] ++
    [f s       | (c, f) <- snd fs, c s]

ping      = isPrefixOf "PING :"
h'        = isPrefixOf "!history"
lb        = isPrefixOf (':' : lambdabot)
cl        = isPrefixOf (':' : clojurebot)
priv      = ("PRIVMSG" ==) . (!! 1) . words

isChan s  = length w > 2 && w !! 2 == chan
    where w = words s

pong x    = write "PONG" (':' : drop 6 x)
resp x    = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)

hist :: String -> MessageStack -> Net ()
hist s stack = hist' (sender s) (reverse (take num stack))
    where
        num
            | isInteger = n
            | otherwise = 50
        c         = drop 9 $ clean s
        n         = read c :: Int
        isInteger = case reads c :: [(Integer, String)] of
            [(_, "")] -> True
            _         -> False

hist'     = mapM_ . sendHistory
addSender = ('<' :) . (++ ">")
