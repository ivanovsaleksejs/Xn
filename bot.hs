{-# LANGUAGE OverloadedStrings #-}

import System.IO

import Control.Monad.Reader as R
import Control.Exception as E

import Prelude hiding (catch)

import Bot.Config
import Bot.Bot

--
-- Set up actions to run on start and enConfigd, and run the main loop
--
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (\e -> const (return ()) (e :: IOException))
