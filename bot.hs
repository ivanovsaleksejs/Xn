{-# LANGUAGE OverloadedStrings #-}

import System.IO

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
exception  = const $ return ((), [] :: MessageStack, ())

main :: IO State
main = do

    stack   <- openLocalStateFrom "chatBase/" (Stack [("", "")])
    history <- query stack (ViewMessages 200)

    bracket open disconnect (\st -> catch (runRWST (run stack) st history) exception)

    where
        disconnect = hClose . socket
        open       = reviveConnection
                     >>= maybe connect return
                     >>= makeBot
                     >>= listenForRestart
