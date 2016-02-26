{-# LANGUAGE DeriveDataTypeable, TypeFamilies, StandaloneDeriving #-}
module Bot.Config.State where

import System.Time

import Data.SafeCopy
import Data.Typeable
import Data.Tuple.Utils
import Data.Acid
import Data.Acid.Advanced

import Control.Monad.RWS
import Control.Concurrent.STM.TChan

import Bot.Config.Basic
import Bot.Config.StateMethods

instance SafeCopy Stack where
    putCopy (Stack list) = contain $ safePut list
    getCopy = contain $ fmap Stack safeGet

data ViewMessages = ViewMessages Int
data AddMessage   = AddMessage Msg
data GetUptime    = GetUptime
data FindTellItem = FindTellItem String
data SaveTellItem = SaveTellItem TellItem

deriving instance Typeable AddMessage
instance SafeCopy AddMessage where
    putCopy (AddMessage st) = contain $ safePut st
    getCopy = contain $ fmap AddMessage safeGet
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

deriving instance Typeable FindTellItem
instance SafeCopy FindTellItem where
    putCopy (FindTellItem st) = contain $ safePut st
    getCopy = contain $ liftM FindTellItem safeGet
instance Method FindTellItem where
    type MethodResult FindTellItem = TellList
    type MethodState FindTellItem = Stack
instance UpdateEvent FindTellItem

deriving instance Typeable SaveTellItem
instance SafeCopy SaveTellItem where
    putCopy (SaveTellItem st) = contain $ safePut st
    getCopy = contain $ fmap SaveTellItem safeGet
instance Method SaveTellItem where
    type MethodResult SaveTellItem = ()
    type MethodState SaveTellItem = Stack
instance UpdateEvent SaveTellItem

instance IsAcidic Stack where
    acidEvents = [ UpdateEvent (\(AddMessage newState) -> addMessage newState)
                 , QueryEvent (\(ViewMessages cnt)     -> viewMessages cnt)
                 , QueryEvent (\GetUptime              -> getUptime)
                 , UpdateEvent (\(FindTellItem tgt)     -> findTellItem tgt)
                 , UpdateEvent (\(SaveTellItem item)   -> saveTellItem item)
                 ]
