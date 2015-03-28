module Bot.Messaging

where

import Data.List
import Data.List.Utils
import Data.Acid

import Control.Applicative
import Control.Arrow
import Control.Monad.IfElse
import Control.Monad.RWS hiding (join)

import System.IO
import System.Time

import Bot.Config
import Bot.General
import Bot.Helpers

import Bot.Commands.History
import Bot.Commands.Str
import Bot.Commands.Rand
import Bot.Commands.Time
import Bot.Commands.URL

commands :: [(String -> Bool, String -> MessageStack -> Net ())]
commands =
    [
            (s' . clean, substmsg), -- Substitute
            (h' . clean, hist)      -- History
    ]
    ++ second (const .) `map` [
        (evlb, privmsg lambdabot  . clean), -- Eval to lambdabot
        (tolb, privmsg lambdabot  . clean), -- Command to lambdabot
        (tocl, privmsg clojurebot . clean), -- Command to clojurebot
        (hasUrls, showTitles), -- Show titles of urls in message
        (ping, pong),          -- Ping
        (lb,   resp),          -- Response from lambdabot
        (cl,   resp)           -- Response from clojurebot
    ]
    ++ [(isPrefixOf cmd . clean, const . f) | (cmd, f) <- cmd]
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

listen :: AcidState (EventState AddMessage) -> Handle -> Net ()
listen acidStack h = forever $ do
    -- Get a line from buffer managed by Handle h
    s  <- init <$> io (hGetLine h)
    -- Output line to stdout for logging
    io $ putStrLn s

    -- Get current system time
    now   <- io nowtime
    -- Get message stack from State monad
    stack <- get

    -- If message is on channel, save it in State monad and acid-state base
    whenM (return $ isChan s) $ do
        let msg = (now, s)
        put $ take 200 $ msg : stack
        io  $ update acidStack (AddMessage msg)

    -- Process line
    head . map (\(_, f) -> f s stack) . filter (($ s) . fst) $ commands
