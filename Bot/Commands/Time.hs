module Bot.Commands.Time

where

import Data.List

import Control.Monad.RWS as R

import System.Time
import Data.Time

import Bot.Config

--
-- Calculate and pretty print the uptime
--
uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

--
-- Get zoned time as IO String
--
nowtime :: IO String
nowtime = fmap (takeWhile (/= '.') . (!!1) . words . show) getZonedTime

--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td = R.join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 12,"m")
    ,(days   `mod` 28,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
    where
        secs    = abs $ tdSec td  ; mins   = secs   `div` 60
        hours   = mins   `div` 60 ; days   = hours  `div` 24
        months  = days   `div` 28 ; years  = months `div` 12
        f (i,s) | i == 0    = []
                | otherwise = show i ++ s
