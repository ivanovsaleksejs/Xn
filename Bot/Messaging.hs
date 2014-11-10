module Bot.Messaging

where

import Data.List
import Data.List.Utils
import Data.Acid

import Control.Monad.RWS hiding (join)
import Control.Applicative

import System.IO

import Bot.Config
import Bot.General
import Bot.Helpers

import Bot.Commands.History
import Bot.Commands.Str
import Bot.Commands.Rand
import Bot.Commands.Time
import Bot.Commands.URL

commands =
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
-- listen :: Handle -> Net ()
listen acidStack h = forever $ do
    -- io :: IO a -> Net a
    -- io = liftIO
    s  <- init <$> io (hGetLine h)
    io $ putStrLn s

    now   <- io nowtime
    stack <- get

    if isChan s then do
        let msg = (now, s)
        put $ take 200 $ msg : stack
        io  $ update acidStack (AddMessage msg)
        process s stack
    else
        process s stack

    where
        process s stack = head $
            [f s stack | (c, f) <- fst commands, c s] ++
            [f s       | (c, f) <- snd commands, c s]
