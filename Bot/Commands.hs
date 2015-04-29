module Bot.Commands

where

import Data.List
import Control.Monad
import Bot.Config
import Bot.General
import Bot.Commands.History
import Bot.Commands.URL

hasUrls   = liftM2 (&&) isChan (null . urls . clean)
evlb s    = (isPrefixOf "> " $ clean s) && sender s /= "daGrevis" -- Yes, this actually is hardcode for daGrevis

-- Lambdabot commands
lbCmd        = [":t", "@free", "@hoogle", "@pl", "@pointful", "@quote", "@undo"]
[tolb, tocl] = map (\x -> flip any x . flip isPrefixOf . clean) [lbCmd, [",("]]

[ping, historyP, substituteP, lb, cl] =
    map isPrefixOf
    ["PING :", "!history", "s/", ':' : lambdabot, ':' : clojurebot]

pong x    = write "PONG" (':' : drop 6 x)
resp x    = write "PRIVMSG " (chan ++ ' ' : ':' : clean x)
isChan s  = length w > 2 && w !! 1 == "PRIVMSG" && w !! 2 == chan
    where w = words s

showTitles s = getTitles c >>= mapM_ (sendDelayed . privmsg t)
    where
        (t, c) = (target s, clean s)
