module Bot.General where

import Prelude hiding (putStrLn, hPutStrLn)
import System.IO

import Bot.Config.Basic
import Bot.Config.State
import Bot.Config.StateMethods
import Control.Monad.RWS
import Control.Concurrent.STM

-- Send a privmsg to the channel/user + server
privmsgPrio :: Bool -> String -> String -> Net ()
privmsgPrio prio target s = do
    chan <- asks $ if prio then quick else slow
    liftIO . atomically $ writeTChan chan (target ++ " :" ++ s)

privmsg = privmsgPrio True

-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    let str = s ++ " " ++ t
    liftIO $ hPutStrLn h str
    liftIO $ putStrLn $ "> "++ str

-- Clear message of prefix
clean     = drop 1 . dropWhile (/= ':') . drop 1

-- Multiple filter
multiFilter = filter . flip (all . flip id)

-- Set target of response
target :: String -> String
target x
    | parts !! 1 == "PRIVMSG" 
    && (not $ elem t [lambdabot, clojurebot])
    && ch /= chan 
        = t
    | otherwise
        = chan
    where
        parts  = words x
        ch     = parts !! 2
        t      = takeWhile (/= '!') $ drop 1 $ parts !! 0

-- Get sender of message
sender :: String -> String
sender x
    | x == ""                 = chan
    | parts !! 1 == "PRIVMSG" = t
    | otherwise               = chan
    where
        parts  = words x
        t      = takeWhile (/= '!') $ drop 1 $ parts !! 0

addSender = ('<' :) . (++ ">")

-- Checks if string is valid Integer number
isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False
