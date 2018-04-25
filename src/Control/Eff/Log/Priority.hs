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
import Control.Eff.Lift (Lifted)
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

logTo :: ( LogMessage l
         , Member (LogM m PriorityLog) r
         , Lifted m r ) => Priority -> l -> Eff r ()
logTo p l = logM (PriorityLog p $ toMsg l)
{-# INLINE logTo #-}

logDebug :: ( LogMessage l
            , Member (LogM m PriorityLog) r
            , Lifted m r ) => l -> Eff r ()
logDebug = logTo DEBUG
{-# INLINE logDebug #-}

logInfo :: ( LogMessage l
           , Member (LogM m PriorityLog) r
           , Lifted m r ) => l -> Eff r ()
logInfo = logTo INFO
{-# INLINE logInfo #-}

logNotice :: ( LogMessage l
             , Member (LogM m PriorityLog) r
             , Lifted m r ) => l -> Eff r ()
logNotice = logTo NOTICE
{-# INLINE logNotice #-}

logWarning :: ( LogMessage l
              , Member (LogM m PriorityLog) r
              , Lifted m r ) => l -> Eff r ()
logWarning = logTo WARNING
{-# INLINE logWarning #-}

logError :: ( LogMessage l
            , Member (LogM m PriorityLog) r
            , Lifted m r ) => l -> Eff r ()
logError = logTo ERROR
{-# INLINE logError #-}

logCritical :: ( LogMessage l
               , Member (LogM m PriorityLog) r
               , Lifted m r ) => l -> Eff r ()
logCritical = logTo CRITICAL
{-# INLINE logCritical #-}

logAlert :: ( LogMessage l
            , Member (LogM m PriorityLog) r
            , Lifted m r ) => l -> Eff r ()
logAlert = logTo ALERT
{-# INLINE logAlert #-}

logEmergency :: ( LogMessage l
                , Member (LogM m PriorityLog) r
                , Lifted m r ) => l -> Eff r ()
logEmergency = logTo EMERGENCY
{-# INLINE logEmergency #-}
