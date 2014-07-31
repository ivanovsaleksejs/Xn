module Bot.Messaging

where

import Data.List

import Control.Monad.RWS

import System.IO

import Bot.Config
import Bot.Helpers

import Bot.Commands.Str
import Bot.Commands.Rand
import Bot.Commands.Time
import Bot.Commands.URL

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    stack <- get
    io $ putStrLn s
    if length (words s) > 2 && (words s) !! 2 == chan then put $ s : (take 100 stack)
    else put $ take 100 stack
    if ping s then pong s
    else if s' (clean s) then substmsg s stack
    else if history (clean s) then hist (sender s) (reverse (take 50 stack))
    else if lb s || cl s then resp s
    else if (words s) !! 1 == "PRIVMSG" then eval (sender s) (target s) (clean s)
    else return ()
    where
        forever a = a >> forever a

--
-- Dispatch a command
--
eval :: String -> String -> String -> Net ()
eval sender target x 
    | x == "!uptime"           = uptime >>= privmsg target
    | x == "!ping"             = privmsg target "pong"
    | "!id "  `isPrefixOf` x   = privmsg target (drop 4 x)
    | "!lb "  `isPrefixOf` x   = privmsg lambdabot (drop 4 x)
    | "!cl "  `isPrefixOf` x   = privmsg clojurebot (drop 4 x)
    | "!rand" `isPrefixOf` x   = rand (drop 6 x) >>= privmsg target 
    | urls any x               = getTitles x >>= privmsg target
    | hasAbbr x                = privmsg target (addSender sender ++ " " ++ replaceAbbr x)
    | otherwise                = return () -- ignore everything else

--
-- Send a privmsg to the channel/user + server
--
privmsg :: String -> String -> Net ()
privmsg target s = write "PRIVMSG" (target ++ " :" ++ s)

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
