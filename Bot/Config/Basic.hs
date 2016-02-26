{-# LANGUAGE DeriveDataTypeable, TypeFamilies, StandaloneDeriving #-}
module Bot.Config.Basic where

import System.IO
import System.Time

import Control.Monad.RWS
import Control.Concurrent.STM.TChan

server     = "irc.freenode.org"
port       = 6667
chan       = "#developerslv"
nick       = "Xn"
password   = "12345"
lambdabot  = "lambdabot"
clojurebot = "clojurebot"
stateDir   = "chatBase/"

type Msg          = (String, String)
type MessageStack = [Msg]
type TellItem     = (String, String, String)
type TellList     = [TellItem]
type State        = (ClockTime, MessageStack, TellList)
type Net          = RWST Bot () MessageStack IO

data Stack  = Stack State
data Bot    = Bot { socket :: Handle, starttime :: ClockTime, quick :: TChan String, slow :: TChan String}
