module Bot.Helpers

where

import Data.List
import Text.Printf

import Control.Monad.RWS
import Control.Concurrent

import System.Random

import Bot.Config

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
history   = isPrefixOf "!history"
lb        = isPrefixOf (':' : lambdabot)
cl        = isPrefixOf (':' : clojurebot)
pong x    = write "PONG" (':' : drop 6 x)
resp x    = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)
hist x    = mapM_ (sendHistory x)
addSender = ('<' :) . (++ ">")

--
-- Send a history entry to user
--
sendHistory :: String -> String -> RWST Bot () MessageStack IO ()
sendHistory x s = do
    write "PRIVMSG " (x ++ ' ' : ':' : '<' : sender s ++ "> " ++ clean s)
    io $ threadDelay =<< fmap ((+) 500000 . flip mod 1000000) randomIO

--
-- Get sender's last message that is not s/ command
--
lastmsg :: String -> MessageStack -> String
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
