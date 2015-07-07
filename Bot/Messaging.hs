module Bot.Messaging

where

import Data.Maybe
import Data.Acid

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.IfElse
import Control.Monad.RWS hiding (join)

import Prelude hiding (putStrLn)
import System.IO hiding (hGetLine, putStrLn)
import System.IO.UTF8

import Bot.Config
import Bot.General
import Bot.Commands
import Bot.Commands.Time

yieldCmd :: a -> (a -> Bool) -> (a -> b) -> (Maybe b)
yieldCmd a cond f
    | cond a    = Just $ f a 
    | otherwise = Nothing

listen :: AcidState (EventState AddMessage) -> Net ()
listen acidStack = forever $ do
    handle <- asks socket
    line   <- fmap init . liftIO . hGetLine $ handle
    now    <- liftIO nowtime
    stack  <- get
    env    <- ask

    liftIO $ putStrLn line
    -- If message is on channel, save it in State monad and acid-state base
    whenM (return $ isChan line) $ do
        let msg = (now, line)
        put    $ take 200 $ msg : stack
        liftIO $ update acidStack (AddMessage msg)

    -- Find a appropriate command to execute.
    let cmd = fromJust . msum . map (uncurry $ yieldCmd line) $ commands

    void . liftIO . forkIO . void $ runRWST cmd env stack

sendOne :: [String] -> Maybe (Net [String])
sendOne msgs
    | msg /= [] = Just (send >> return rest)
    | otherwise = Nothing
    where
        (msg, rest) = splitAt 1 msgs
        send        = write "PRIVMSG" (head $ msg)

forwardOutput = forwardOutput' [] []
forwardOutput' :: [String] -> [String] -> Net ()
forwardOutput' quick slow = do
    chan <- asks out

    let store msg = uncurry forwardOutput' $ update msg (quick, slow)
    let tryQuick  = sendOne quick >>= return . fmap (flip (,) slow)
    let trySlow   = sendOne slow  >>= return . fmap ((,) quick)
    let timeout _ = (return ([], []) `fromMaybe` trySlow) `fromMaybe` tryQuick

    winner <- liftIO $ race (threadDelay 50000) (readChan chan)
    either ((uncurry forwardOutput' =<<) . timeout) store winner

    where
        choice True        = first
        choice False       = second
        update (cond, msg) = choice cond (msg :)
