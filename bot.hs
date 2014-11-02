{-# LANGUAGE OverloadedStrings #-}

import System.IO

import Control.Monad.RWS
import Control.Exception

import Prelude hiding (catch)

import Data.Maybe

import Bot.Restarter
import Bot.Config
import Bot.Bot

--
-- Set up actions to run on start and end, and run the main loop
--

msgStack :: MessageStack
msgStack = [("", "")]

main :: IO ((), MessageStack, ())
main = bracket open disconnect loop
  where
    open       = reviveConnection >>= maybe connect return >>= makeBot >>= listenForRestart
    disconnect = hClose . socket
    loop st    = catch (runRWST run st msgStack) (\e -> const(return((),([] :: MessageStack),()))  (e :: IOException))
