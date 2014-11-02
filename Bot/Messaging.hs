module Bot.Messaging

where

import Data.List
import Data.List.Utils

import Control.Monad.RWS hiding (join)

import System.IO

import Bot.Config
import Bot.General
import Bot.Helpers

import Bot.Commands.History
import Bot.Commands.Str
import Bot.Commands.Rand
import Bot.Commands.Time
import Bot.Commands.URL

pairs =
    (
        [
            (s' . clean, substmsg), -- Substitute
            (h' . clean, hist)      -- History
        ],
        [
            (tolb, privmsg lambdabot . clean), -- Command to lambdabot
            (hasUrls, showTitles), -- Show titles of urls in message
            (ping, pong),          -- Ping
            (lb,   resp),          -- Response from lambdabot
            (cl,   resp)           -- Response from clojurebot
        ]
        ++ [ (c, f) | x <- cmd, let c = isPrefixOf (fst x) . clean, let f = snd x]
    )
    where
        cmd = [
                ("!id",     ap pm d4),                  -- Show string
                ("!ab",     ap pm ab),                  -- Replace abbrs
                ("!uptime", (uptime >>=) . pm),         -- Show uptime
                ("!ping",   flip pm "pong"),            -- Show "pong"
                ("!lb",     privmsg lambdabot . d4),    -- Command to lambdabot
                ("!cl",     privmsg clojurebot . d4),   -- Command to clojurebot
                ("!rand",   ap ((>>=) . rand . d6) pm), -- Show random number
                ("",        const $ return ())
            ]
        [d2, d4, d6] = map ((. clean) . drop) [2,4,6]
        pm       = privmsg . target
        ab s     = join " " $ map ($ s) [addSender . sender, replaceAbbr . d4]

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $
    -- Get a line from buffer managed by Handle h
    init `fmap` io (hGetLine h) >>=

    (\s ->
        -- Output line to stdout for logging
        (io $ putStrLn s) >>
        -- Get current system time
        io nowtime >>=
        -- Get message stack
        \now -> get >>=

        (\stack ->
            -- Save message in stack
            (put $ filter (isChan . snd) [(now, s)] ++ take 200 stack) >>
            -- Process line
            helper s stack pairs
        )
    )
