{-# LANGUAGE DeriveDataTypeable, TypeFamilies, StandaloneDeriving #-}
module Bot.Config

where

import System.IO
import System.Time
import System.Environment

import Data.SafeCopy
import Data.Typeable
import Data.Tuple.Utils
import Data.Acid
import Data.Acid.Advanced

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.RWS

server     = "irc.freenode.org"
port       = 6667
chan       = "#developerslv"
nick       = "Xn_pls"
lambdabot  = "lambdabot"
clojurebot = "clojurebot"

type Msg          = (String, String)
type MessageStack = [Msg]

type State = (ClockTime, MessageStack, ())
type Net   = RWST Bot () MessageStack IO

data Bot   = Bot { socket :: Handle, starttime :: ClockTime }
data Stack = Stack State

instance SafeCopy Stack where
    putCopy (Stack list) = contain $ safePut list
    getCopy = contain $ Stack <$> safeGet

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
    put $ Stack (f, msg : s, t)

viewMessages :: Int -> Query Stack MessageStack
viewMessages limit = do
    Stack state <- ask
    return . take limit $ snd3 state

data ViewMessages = ViewMessages Int
data AddMessage   = AddMessage Msg
data GetUptime    = GetUptime

deriving instance Typeable AddMessage
instance SafeCopy AddMessage where
    putCopy (AddMessage st) = contain $ safePut st
    getCopy = contain $ AddMessage <$> safeGet
instance Method AddMessage where
    type MethodResult AddMessage = ()
    type MethodState AddMessage = Stack
instance UpdateEvent AddMessage

deriving instance Typeable ViewMessages
instance SafeCopy ViewMessages where
    putCopy (ViewMessages cnt) = contain $ return ()
    getCopy = contain $ liftM ViewMessages safeGet
instance Method ViewMessages where
    type MethodResult ViewMessages = MessageStack
    type MethodState ViewMessages = Stack
instance QueryEvent ViewMessages

deriving instance Typeable ClockTime
deriving instance Typeable GetUptime
instance SafeCopy GetUptime where
    putCopy GetUptime = contain $ return ()
    getCopy = contain $ return GetUptime
instance Method GetUptime where
    type MethodResult GetUptime = ClockTime
    type MethodState GetUptime = Stack
instance QueryEvent GetUptime

instance IsAcidic Stack where
    acidEvents = [ UpdateEvent (\(AddMessage newState) -> addMessage newState)
                 , QueryEvent (\(ViewMessages cnt)     -> viewMessages cnt)
                 , QueryEvent (\GetUptime              -> getUptime)
                 ]
