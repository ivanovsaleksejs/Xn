module Bot.Commands where

import Data.List
import Data.List.Utils
import Control.Monad hiding (join)

import Bot.Config.Basic
import Bot.Config.State
import Bot.Config.StateMethods
import Bot.General

import Bot.Commands.History
import Bot.Commands.Morse
import Bot.Commands.Str
import Bot.Commands.Rand
import Bot.Commands.Time
import Bot.Commands.URL

hasUrls   = liftM2 (&&) isChan (not . null . urls . clean)
evlb s    = (isPrefixOf "> " $ clean s) && sender s /= "daGrevis" -- Yes, this actually is hardcode for daGrevis

-- Lambdabot commands
lbCmd        = [":t", "@free", "@hoogle", "@pl", "@pointful", "@quote", "@undo", "@src"]
[tolb, tocl] = map (\x -> flip any x . flip isPrefixOf . clean) [lbCmd, [",("]]

[ping, historyP, substituteP, lb, cl, tellP] =
    map isPrefixOf
    ["PING :", "!history", "s/", ':' : lambdabot, ':' : clojurebot, "!tell"]

pong x   = write "PONG" (':' : drop 6 x)
resp x   = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)
isChan s = length w > 2 && w !! 1 == "PRIVMSG" && w !! 2 == chan
    where w = words s

showTitles :: String -> Net ()
showTitles s = getTitles c >>= mapM_ (privmsg t)
    where
        (t, c) = (target s, clean s)
        
commands :: [(String -> Bool, String -> Net ())]
commands =
    [
        (substituteP. clean, substitute),
        (historyP   . clean, history)
    ]
    ++ [
        (evlb, privmsg lambdabot  . clean), -- Eval to lambdabot
        (tolb, privmsg lambdabot  . clean), -- Command to lambdabot
        (tocl, privmsg clojurebot . clean), -- Command to clojurebot
        (hasUrls, showTitles), -- Show titles of urls in message
        (ping, pong),          -- Ping
        (lb,   resp),          -- Response from lambdabot
        (cl,   resp)           -- Response from clojurebot
    ]
    ++ [(isPrefixOf cmd . clean, f) | (cmd, f) <- cmd]
    where
        cmd = [
                ("!id",     ap pm d4),                  -- Show string
                ("!ab",     ap pm ab),                  -- Replace abbrs
                ("!fm",     ap pm fm),                  -- Translate from Morse
                ("!tm",     ap pm tm),                  -- Translate to Morse
                ("!uptime", (uptime >>=) . pm),         -- Show uptime
                ("!ping",   flip pm "pong"),            -- Show "pong"
                ("!lb",     privmsg lambdabot . d4),    -- Command to lambdabot
                ("!cl",     privmsg clojurebot . d4),   -- Command to clojurebot
                ("!rand",   ap ((>>=) . rand . d6) pm)  -- Show random number
            ]
        [d2, d4, d6] = map ((. clean) . drop) [2,4,6]
        pm       = privmsg . target
        [fm, tm] = map (. d4) [fromMorse, toMorse]
        ab s     = join " " $ map ($ s) [addSender . sender, replaceAbbr . d4]
