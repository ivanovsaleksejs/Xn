module Bot.Commands.Rand where

import System.Random
import Control.Monad.RWS

import Bot.Config.Basic
import Bot.Config.State
import Bot.Config.StateMethods
import Bot.General

-- Generate a random Integer in range 0..n
rand :: String -> Net String
rand s
    | not  (isInteger s) || n == 0 = return "y u do dis"
    | otherwise                    = liftIO $ fmap (show . flip mod n) randomIO
    where
        n           = read s :: Int
