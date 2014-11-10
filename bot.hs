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

msgStack :: MessageStack
msgStack = [("", "")]

main :: IO ((), MessageStack, ())
main = do

    stack <- openLocalStateFrom "chatBase/" (Stack [("", "")])

    history <- query stack (ViewMessages 200)

    bracket open disconnect (\st -> catch (runRWST (run stack) st history) (\e -> const(return((),([] :: MessageStack),()))  (e :: IOException)))--loop

    where
        open       = reviveConnection >>= maybe connect return >>= makeBot >>= listenForRestart
        disconnect = hClose . socket
