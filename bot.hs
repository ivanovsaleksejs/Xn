{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.Text (unpack)
import Data.Monoid (mconcat)

import Network
import Network.HTTP.Conduit (simpleHttp)

import System.IO
import System.Time
import System.Exit
import System.Random

import Control.Monad.Reader
import Control.OldException
import Text.Printf
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (attributeIs, content, element,
                         fromDocument, ($//), (&//), (>=>))

import Prelude hiding (catch)

server = "irc.freenode.org"
port   = 6667
chan   = "#developerslv"
nick   = "Xn_pls"
 
--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }
 
--
-- Set up actions to run on start and end, and run the main loop
--
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (const $ return ())
 
--
-- Connect to the server and return the initial bot state
--
connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a
 
--
-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
--
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen
 
--
-- Process each line from the server
--
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else if lb s || cl s then resp s else  eval (clean s)
	  where
	    forever a = a >> forever a
	    clean     = drop 1 . dropWhile (/= ':') . drop 1
	    ping x    = "PING :" `isPrefixOf` x
	    lb x      = ":lambdabot" `isPrefixOf` x
	    cl x      = ":clojurebot" `isPrefixOf` x
	    pong x    = write "PONG" (':' : drop 6 x)
	    resp x    = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)
 
--
-- Dispatch a command
--
eval :: String -> Net ()
eval     "!uptime"             = uptime >>= privmsg chan
eval     "!ping"               = privmsg chan "pong"
eval     "!quit"               = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval x 
    | "!id " `isPrefixOf` x = privmsg chan (drop 4 x)
    | "!lb " `isPrefixOf` x = privmsg "lambdabot" (drop 4 x)
    | "!cl " `isPrefixOf` x = privmsg "clojurebot" (drop 4 x)
    | "!rand" `isPrefixOf` x   = rand (drop 6 x) >>= privmsg chan 
    | "http://" `isPrefixOf` x = fetchTitle x >>= privmsg chan
eval     _                     = return () -- ignore everything else
 
--
-- Send a privmsg to the channel/user + server
--
privmsg :: String -> String -> Net ()
privmsg target s = write "PRIVMSG" (target ++ " :" ++ s)

--
-- Fetch a title from web page
--
fetchTitle :: MonadIO m => String -> m String
fetchTitle url = do
    lbs <- simpleHttp url
    let doc = parseLBS lbs
        cursor = fromDocument doc
    return . unpack . mconcat $ cursor $// element "title" &// content

--
-- Generate a random Integer in range 0..n
--
rand :: String -> Net String
rand n = io $ fmap (show . flip mod (read n ::Int)) randomIO

--
-- Send a message out to the server we're currently connected to
--
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t
 
--
-- Calculate and pretty print the uptime
--
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero
 
--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s
 
--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO
