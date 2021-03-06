import System.IO
import System.Time

import Control.Concurrent.Async
import Control.Monad.RWS hiding (listen)
import Control.Exception

import Prelude hiding (catch)

import Data.Acid

import Bot.Restarter
import Bot.Config.Basic
import Bot.Config.State
import Bot.Config.StateMethods
import Bot.Bot
import Bot.Messaging

-- Set up actions to run on start and end, and run the main loop
ignore :: IOException -> IO ()
ignore _ = return ()

main :: IO ()
main = do
    now     <- getClockTime
    stack   <- openLocalStateFrom stateDir (Stack (now, [("", "")], []))
    uptime  <- query stack GetUptime
    history <- query stack (ViewMessages 200)

    let run bot     = env forwardOutput `concurrently` env (ident >> listen stack)
            where env fn = runRWST fn bot history
    bracket (open uptime) disconnect (flip catch ignore . void . run)

    where
        disconnect = hClose . socket
        open time  = reviveConnection
                     >>= maybe connect return
                     >>= makeBot time
                     >>= listenForRestart
