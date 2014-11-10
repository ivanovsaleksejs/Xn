{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Time

import Control.Monad.RWS
import Control.Exception

import Prelude hiding (catch)

import Data.Maybe
import Data.Acid

import Bot.Restarter
import Bot.Config
import Bot.Bot

--
-- Set up actions to run on start and end, and run the main loop
--

exception :: IOException -> IO State
exception e = do
    time <- getClockTime
    return (time, [] :: MessageStack, ())

main :: IO State
main = do

    time    <- getClockTime
    stack   <- openLocalStateFrom "chatBase/" (Stack (time, [("", "")], ()))
    stTime  <- query stack GetUptime
    history <- query stack (ViewMessages 200)

    bracket (open stTime) disconnect (\st -> catch (runRWST (run stack) st history) exception)

    where
        disconnect = hClose . socket
        open time  = reviveConnection
                     >>= maybe connect return
                     >>= makeBot time
                     >>= listenForRestart
