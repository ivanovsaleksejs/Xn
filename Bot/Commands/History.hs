module Bot.Commands.History

where

import Control.Concurrent
import Control.Monad.RWS

import System.Random

import Bot.Config
import Bot.General

history :: String -> Net ()
history s = do
    stack <- get
    mapM_ (sendHistory $ sender s) (reverse (take num stack))
    where
        num
            | isInteger c = n
            | otherwise   = 50
        c           = drop 9 $ clean s
        n           = read c :: Int

-- Send a history entry to user
sendHistory :: String -> Msg -> Net ()
sendHistory target (time, s) = sendDelayed $ privmsgPrio False target msg
    where
        (origin, body) = (sender s, clean s)
        msg            = concat [time, " <", origin, "> ", body]

-- Sends multiple messages with random delay
sendDelayed :: Net a -> Net ()
sendDelayed = (>> (liftIO $ threadDelay =<< fmap interval randomIO))
    where
        interval = (*10^5) . (+) 5 . flip mod 10
