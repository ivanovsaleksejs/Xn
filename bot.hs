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

exception :: IOException -> IO ((), MessageStack, ())
exception _ = return ((), [] :: MessageStack, ())

main = do
    now     <- getClockTime
    stack   <- openLocalStateFrom "chatBase/" (Stack (now, [("", "")], ()))
    uptime  <- query stack GetUptime
    history <- query stack (ViewMessages 200)

    bracket (open uptime) disconnect (\st -> catch (runRWST (run stack) st history) exception)

    where
        disconnect = hClose . socket
        open time  = reviveConnection
                     >>= maybe connect return
                     >>= makeBot time
                     >>= listenForRestart
