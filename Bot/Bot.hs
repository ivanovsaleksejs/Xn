module Bot.Bot

where

import Data.Acid
import Text.Printf

import Control.Monad.RWS hiding (listen)
import Control.Exception

import System.IO
import System.Time

import Network

import Bot.Config
import Bot.General
import Bot.Messaging

--
-- Connect to the server and return the initial bot state
--
connect :: IO Handle
connect = connectTo server (PortNumber (fromIntegral port))

makeBot :: ClockTime -> Handle -> IO Bot
makeBot time h = notify $ do
    t <- getClockTime
    hSetBuffering h NoBuffering
    hSetEncoding  h utf8
    return $ Bot h time
        where
            notify a = bracket_
                (printf "Connecting to %s ... " server >> hFlush stdout)
                (putStrLn "done.")
                a

--
-- Join a channel, and start processing commands
--
run :: AcidState (EventState AddMessage) -> Net ClockTime
run acidStack = do
    write "NICK" nick
    write "USER" (nick ++" 0 * :" ++ chan ++ " channel bot")
    write "JOIN" chan
    asks socket >>= listen acidStack
