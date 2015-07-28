module Bot.Commands.History where

import Control.Monad.RWS

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
sendHistory target (time, s) = privmsgPrio False target msg
    where
        (origin, body) = (sender s, clean s)
        msg            = concat [time, " <", origin, "> ", body]
