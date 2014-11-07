module Bot.Messaging

where

import Data.List
import Data.List.Utils
import Data.Acid

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
            (evlb, privmsg lambdabot  . d1),    -- Eval to lambdabot
            (tolb, privmsg lambdabot  . clean), -- Command to lambdabot
            (tocl, privmsg clojurebot . clean), -- Command to clojurebot
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
        [d1, d2, d4, d6] = map ((. clean) . drop) [1,2,4,6]
        pm       = privmsg . target
        ab s     = join " " $ map ($ s) [addSender . sender, replaceAbbr . d4]

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    -- Get a line from buffer managed by Handle h
    s <- init `fmap` io (hGetLine h)

        -- Output line to stdout for logging
    io $ putStrLn s
        -- Get current system time
    now <- io nowtime
        -- Get message stack
    stack <- io $ openLocalStateFrom "chatBase/" (Stack [("", "")])
            -- Save message in stack
--    put $ take 200 $ filter (isChan . snd) [(now, s)] ++ stack
    io $ update stack (AddMessage (now, s))

    history <- io $ query stack (ViewMessages 200)
            -- Process line
    helper s history pairs
