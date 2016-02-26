module Bot.Commands.Tell where

import System.Time

import Data.Acid
import Data.List.Split
import Data.Tuple.Utils

import Control.Monad.RWS hiding (join)

import Bot.General

import Bot.Config.Basic
import Bot.Config.State
import Bot.Config.StateMethods

tellAdd :: AcidState (EventState SaveTellItem) -> String -> Net ()
tellAdd stack s = do
    let
        a    = splitOn " " $ drop 6 $ clean s
        sndr = sender s
        chan = target s
        (author, tgt, msg) = (sndr, head a, unwords $ tail a)
    case () of
        _ | tgt == "" && msg == "" ->
            privmsg chan $ "Make up your mind, " ++ author ++"!"
        _ | msg == "" -> do
            liftIO $ update stack (SaveTellItem (author, tgt, msg))
            privmsg chan $ "OK " ++ author ++", I won't tell " ++ tgt ++ " anytning"
        _ | otherwise -> do
            liftIO $ update stack (SaveTellItem (author, tgt, msg))
            privmsg chan $ "OK " ++ author ++", I will tell " ++ tgt ++ " once they is on"

tellDump :: AcidState (EventState FindTellItem) -> String -> Net ()
tellDump stack s = do
    let
        sndr = sender s
        tgt  = target s
    tl <- liftIO $ update stack (FindTellItem sndr)
    mapM_ (privmsg tgt . makeMsg) $ filter (\i -> thd3 i /= "") tl
        where
            makeMsg (a, t, m) = t ++ ", " ++ a ++ " left you a message: " ++ m
