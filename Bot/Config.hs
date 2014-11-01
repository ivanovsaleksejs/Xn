module Bot.Config

where

import System.IO
import System.Time

import Control.Monad.RWS

server     = "irc.freenode.org"
port       = 6667
chan       = "#developerslv"
nick       = "Xn_pls"
lambdabot  = "lambdabot"
clojurebot = "clojurebot"

--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Msg = (IO String, String)
type MessageStack = [Msg]
type Net = RWST Bot () MessageStack IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }

--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO
