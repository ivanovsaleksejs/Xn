{-# LANGUAGE OverloadedStrings #-}

import System.IO

import Control.Monad.RWS
import Control.Exception

import Prelude hiding (catch)

import Bot.Config
import Bot.Bot

--
-- Set up actions to run on start and end, and run the main loop
--

msgStack :: MessageStack
msgStack = [(return "", "")]

main :: IO ((), MessageStack, ())
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runRWST run st msgStack) (\e -> const(return((),([] :: MessageStack),()))  (e :: IOException))
