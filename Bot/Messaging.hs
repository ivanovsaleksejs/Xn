module Bot.Messaging

where

import Data.List

import Control.Monad.RWS

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
            (h' . clean, hist) -- History
        ],
        [
            (hasUrls, showTitles), 
            (ping, pong), -- Ping
            (lb,   resp), -- Response from lambdabot
            (cl,   resp), -- Response from clojurebot
            (priv, eval), -- Other commands
            (const True, const $ return ()) -- Nothing
        ]
    )

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    -- Get line and output to stdout
    s <- init `fmap` io (hGetLine h)
    io $ putStrLn s

    -- Save line in stack
    stack <- get
    put $ filter (isChan . snd) [(nowtime, s)] ++ take 200 stack

    -- Process line
    helper s stack pairs

    where
        forever a = a >> forever a

--
-- Dispatch a command
--
eval :: String -> Net ()
eval x
    | c == "!uptime"           = uptime >>= privmsg t
    | c == "!ping"             = privmsg t "pong"
    | "!id "  `isPrefixOf` c   = privmsg t (drop 4 c)
    | "!lb "  `isPrefixOf` c   = privmsg lambdabot (drop 4 c)
    | "!cl "  `isPrefixOf` c   = privmsg clojurebot (drop 4 c)
    | "!rand" `isPrefixOf` c   = rand (drop 6 c) >>= privmsg t
    | "!ab "  `isPrefixOf` c   = privmsg t (addSender s ++ " " ++ replaceAbbr (drop 4 c))
    | otherwise                = return () -- ignore everything else
    where
        (s, t, c) = (sender x, target x, clean x)

--
-- Send a substituted message
--
substmsg :: String -> MessageStack -> Net ()
substmsg s stack = privmsg tgt (addSender author ++ " " ++  subst pattn last)
    where
        tgt    = target s
        author = sender s
        last   = clean $ lastmsg author stack
        pattn  = drop 2 $ clean s
