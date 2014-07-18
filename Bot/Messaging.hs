module Bot.Messaging

where

import Data.List
import Text.Printf

import Control.Monad.Reader as R

import System.IO

import Bot.Config
import Bot.Commands

--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s 
        else if lb s || cl s then resp s 
        else if (words s) !! 1 == "PRIVMSG" then eval (target s) (clean s) 
        else eval "" ""
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
lb        = isPrefixOf (':' : lambdabot)
cl        = isPrefixOf (':' : clojurebot)
pong x    = write "PONG" (':' : drop 6 x)
resp x    = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)

--
-- Get sender of message
--
target x  = if parts !! 1 == "PRIVMSG" && t /= lambdabot && t /= clojurebot && ch /= chan then t else chan
    where 
        parts  = words x
        ch     = parts !! 2
        t      = takeWhile (/= '!') $ drop 1 $ parts !! 0

--
-- Dispatch a command
--
eval :: String -> String -> Net ()
eval target x 
    | x == "!uptime"           = uptime >>= privmsg target
    | x == "!ping"             = privmsg target "pong"
    | "!id "  `isPrefixOf` x   = privmsg target (drop 4 x)
    | "!lb "  `isPrefixOf` x   = privmsg lambdabot (drop 4 x)
    | "!cl "  `isPrefixOf` x   = privmsg clojurebot (drop 4 x)
    | "!rand" `isPrefixOf` x   = rand (drop 6 x) >>= privmsg target 
    | urls any x               = getTitles x >>= privmsg target
    | otherwise                = return () -- ignore everything else

--
-- Send a privmsg to the channel/user + server
--
privmsg :: String -> String -> Net ()
privmsg target s = write "PRIVMSG" (target ++ " :" ++ s)
