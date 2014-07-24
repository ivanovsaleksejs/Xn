module Bot.Messaging

where

import Data.List
import Text.Printf

import Control.Monad.RWS as R

import System.IO

import Bot.Config

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
    put $ s : (take 100 stack)
    io (putStrLn s)
    if ping s then pong s
    else if s' (clean s) then substmsg s stack
    else if lb s || cl s then resp s
    else if (words s) !! 1 == "PRIVMSG" then eval (sender s) (target s) (clean s)
    else return ()
    where
        forever a = a >> forever a

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

--
-- Helpers
--
clean     = drop 1 . dropWhile (/= ':') . drop 1
ping      = isPrefixOf "PING :"
s'        = isPrefixOf "s/"
lb        = isPrefixOf (':' : lambdabot)
cl        = isPrefixOf (':' : clojurebot)
pong x    = write "PONG" (':' : drop 6 x)
resp x    = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)
addSender = ('<' :) . (++ ">")

--
-- Get sender's last message that is not s/ command
--
lastmsg :: String -> [String] -> String
lastmsg a stack
    | length f == 0 = ""
    | otherwise     = head f
    where
        f = filter (\s -> a == (sender s) && not (s' (clean s))) stack

--
-- Set target of response
--
target :: String -> String
target x  = if parts !! 1 == "PRIVMSG" && t /= lambdabot && t /= clojurebot && ch /= chan then t else chan
    where 
        parts  = words x
        ch     = parts !! 2
        t      = takeWhile (/= '!') $ drop 1 $ parts !! 0

--
-- Get sender of message
--
sender :: String -> String
sender x
    | x == ""                 = chan
    | parts !! 1 == "PRIVMSG" = t
    | otherwise               = chan
    where 
        parts  = words x
        t      = takeWhile (/= '!') $ drop 1 $ parts !! 0

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
    | hasAbbr x                = privmsg target (addSender sender ++ replaceAbbr x)
    | otherwise                = return () -- ignore everything else

--
-- Send a privmsg to the channel/user + server
--
privmsg :: String -> String -> Net ()
privmsg target s = write "PRIVMSG" (target ++ " :" ++ s)

--
-- Send a substituted message
--
substmsg s stack = privmsg tgt (addSender author ++ " " ++  subst pattn last)
    where
        tgt    = target s
        author = sender s
        last   = clean $ lastmsg author stack
        pattn  = drop 2 $ clean s
