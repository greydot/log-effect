{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Char (toLower)
import Data.Monoid           ((<>))
import Data.Typeable         (Typeable)

data Priority =
    DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | EMERGENCY
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

data PriorityLog = PriorityLog !Priority
                               !ByteString

instance LogMessage PriorityLog where
  toMsg (PriorityLog p m) = mconcat [ "["
                                    , Char8.pack $ map toLower $ show p
                                    , "] "
                                    , m
                                    ]

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
