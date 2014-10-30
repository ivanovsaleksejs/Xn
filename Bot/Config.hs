module Bot.Config

where

import Text.Printf

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

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

--
-- Clear message of prefix
--
clean     = drop 1 . dropWhile (/= ':') . drop 1
