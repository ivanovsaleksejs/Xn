{-# LANGUAGE DeriveDataTypeable, TypeFamilies, StandaloneDeriving #-}
module Bot.Config.StateMethods where

import System.Time

import Data.SafeCopy
import Data.Typeable
import Data.Tuple.Utils
import Data.Acid
import Data.Acid.Advanced

import Control.Monad.RWS
import Control.Concurrent.STM.TChan

import Bot.Config.Basic

resetUptime :: ClockTime -> Update Stack ()
resetUptime time = do
    Stack messages <- get
    let (f, s, t) = messages
    put $ Stack (time, s, t)

getUptime :: Query Stack ClockTime
getUptime = do
    Stack messages <- ask
    return $ fst3 messages

addMessage :: Msg -> Update Stack ()
addMessage msg = do
    Stack messages <- get
    let (f, s, t) = messages
    put $ Stack (f, msg : take 200 s, t)

viewMessages :: Int -> Query Stack MessageStack
viewMessages limit = do
    Stack state <- ask
    return . take limit $ snd3 state

findTellItem :: String -> Update Stack TellList
findTellItem target = do
    Stack state <- get
    let (f, s, t) = state
    put $ Stack (f, s,  filter (\i -> snd3 i /= target) t)
    return $ filter (\i -> snd3 i == target) t

saveTellItem :: TellItem -> Update Stack ()
saveTellItem item = do
    Stack messages <- get
    let (f, s, t) = messages
    let (a, n, _) = item
    put $ Stack (f, s,  item : filter (\i -> not $ (fst3 i == a && snd3 i == n)) t)
