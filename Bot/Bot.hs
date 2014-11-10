module Bot.Bot

where

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
connect = connectTo server (PortNumber (fromIntegral port))

makeBot :: Handle -> IO Bot
makeBot h = notify $ do
    t <- getClockTime
    hSetBuffering h NoBuffering
    hSetEncoding  h utf8
    return $ Bot h t
        where
            notify a = bracket_
                (printf "Connecting to %s ... " server >> hFlush stdout)
                (putStrLn "done.")
                a

--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
-- run :: Net ()
run acidStack = do
    write "NICK" nick
    write "USER" (nick ++" 0 * :" ++ chan ++ " channel bot")
    write "JOIN" chan
    asks socket >>= listen acidStack
