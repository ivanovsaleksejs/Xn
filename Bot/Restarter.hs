module Bot.Restarter

where

import Control.Applicative

import Data.Functor
import Data.Traversable as T (sequence)

import Safe.Exact

import System.Environment
import System.IO
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Time

import Bot.Config

-- executeFile :: Command -> Maybe Search PATH -> Arguments -> Maybe ArgumentsEnvironment -> IO a
restart :: Int -> IO ()
restart fd = getProgName >>= (\name -> executeFile name False [show fd] Nothing)

listenForRestart :: Bot -> IO Bot
listenForRestart bot = installHandler sigUSR1 handle Nothing >> return bot
    where handle = Catch $ handleToFd (socket bot) >>= restart . fromIntegral

reviveConnection :: IO (Maybe Handle)
reviveConnection = do
    args <- getArgs

    T.sequence =<< ((fdToHandle . read . head <$>) . takeExactMay 1 <$> getArgs)
