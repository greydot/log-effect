{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
module Control.Eff.Log.Priority ( PriorityLog(..)
                                , Priority(..)
                                , logTo
                                , logDebug
                                , logInfo
                                , logNotice
                                , logWarning
                                , logError
                                , logCritical
                                , logAlert
                                , logEmergency
                                ) where

import Control.Eff           (Eff, Member)
import Control.Eff.Log
import Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Monoid           ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable         (Typeable)

class LogMessage l where
  toMsg :: l -> ByteString

instance LogMessage ByteString where
  toMsg = id

instance LogMessage [Char] where
  toMsg = Char8.pack

instance LogMessage Text where
  toMsg = encodeUtf8

data Priority =
    DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | EMERGENCY
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

data PriorityLog = PriorityLog !Priority
                               !ByteString

logTo :: (LogMessage l, Member (Log PriorityLog) r)
  => Priority -> l -> Eff r ()
logTo p l = logE (PriorityLog p $ toMsg l)
{-# INLINE logTo #-}

logDebug :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logDebug = logTo DEBUG

logInfo :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logInfo = logTo INFO

logNotice :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logNotice = logTo NOTICE

logWarning :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logWarning = logTo WARNING

logError :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logError = logTo ERROR

logCritical :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logCritical = logTo CRITICAL

logAlert :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logAlert = logTo ALERT

logEmergency :: (LogMessage l, Member (Log PriorityLog) r) => l -> Eff r ()
logEmergency = logTo EMERGENCY
