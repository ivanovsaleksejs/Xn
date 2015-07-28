module Bot.Bot where

import Text.Printf

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.RWS hiding (listen)
import Control.Exception

import System.IO
import System.Time

import Network

import Bot.Config
import Bot.General
import Bot.Messaging

-- Connect to the server and return the initial bot state
connect :: IO Handle
connect = connectTo server (PortNumber (fromIntegral port))

makeBot :: ClockTime -> Handle -> IO Bot
makeBot time h = notify $ do
    q <- newTChanIO
    s <- newTChanIO
    t <- getClockTime
    hSetBuffering h NoBuffering
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    return $ Bot h time q s
        where
            notify a = bracket_
                (printf "Connecting to %s ... " server >> hFlush stdout)
                (putStrLn "done.")
                a

-- Join a channel, and start processing commands
ident :: Net ()
ident = do
    write "NICK" nick
    write "USER" (nick ++" 0 * :" ++ chan ++ " channel bot")
    write "PRIVMSG" $ "NickServ :identify " ++ password
    liftIO $ threadDelay 20000000
    write "JOIN" chan
