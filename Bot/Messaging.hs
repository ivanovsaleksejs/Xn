module Bot.Messaging

where

import Data.List
import Data.List.Utils
import Data.Maybe
import Data.Acid

import Control.Applicative
import Control.Arrow
import Control.Concurrent
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

commands :: [(String -> Bool, String -> Net ())]
commands =
    [
            (s' . clean, substitute),
            (h' . clean, history)
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

yieldCmd :: a -> (a -> Bool) -> (a -> b) -> (Maybe b)
yieldCmd a cond f = if cond a then Just $ f a else Nothing

listen :: AcidState (EventState AddMessage) -> Handle -> Net ()
listen acidStack h = forever $ do
    line  <- fmap init . io . hGetLine $ h
    now   <- io nowtime
    stack <- get
    env   <- ask

    io $ putStrLn line
    -- If message is on channel, save it in State monad and acid-state base
    whenM (return $ isChan line) $ do
        let msg = (now, line)
        put $ take 200 $ msg : stack
        io  $ update acidStack (AddMessage msg)

    -- Find a appropriate command to execute.
    let cmd = head . catMaybes . map (uncurry $ yieldCmd line) $ commands

    void . io . forkIO . void $ runRWST cmd env stack
