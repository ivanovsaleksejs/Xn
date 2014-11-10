{-# LANGUAGE DeriveDataTypeable, TypeFamilies, StandaloneDeriving #-}
module Bot.Config

where

import System.IO
import System.Time
import System.Environment

import Data.SafeCopy
import Data.Typeable
import Data.Acid
import Data.Acid.Advanced

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS


server     = "irc.freenode.org"
port       = 6667
chan       = "#xn_camp"
nick       = "Xn_kthx"
lambdabot  = "lambdabot"
clojurebot = "clojurebot"

--
-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
-- A socket and the bot's start time.
--
type Msg = (String, String)
type MessageStack = [Msg]
type State = ((), MessageStack, ())
type Net = RWST Bot () MessageStack IO

data Stack = Stack MessageStack
data Bot = Bot { socket :: Handle, starttime :: ClockTime }

instance SafeCopy Stack where
    putCopy (Stack list) = contain $ safePut list
    getCopy = contain $ Stack <$> safeGet

addMessage :: Msg -> Update Stack ()
addMessage msg = get >>= \ a -> case a of { Stack messages -> put $ Stack (msg : messages); _ -> fail ""}

viewMessages :: Int -> Query Stack MessageStack
viewMessages limit = ask >>= \ a -> case a of { Stack messages -> return $ take limit messages; _ -> fail ""}

data ViewMessages = ViewMessages Int
data AddMessage   = AddMessage Msg

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

instance IsAcidic Stack where
    acidEvents = [ UpdateEvent (\(AddMessage newState) -> addMessage newState)
                 , QueryEvent (\(ViewMessages cnt)     -> viewMessages cnt)
                 ]

--
-- Convenience.
--
io :: IO a -> Net a
io = liftIO
