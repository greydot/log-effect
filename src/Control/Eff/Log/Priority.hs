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
import Data.Monoid           ((<>))
import Data.Typeable         (Typeable)

data Priority =
    DEBUG | INFO | NOTICE | WARNING | ERROR | CRITICAL | ALERT | EMERGENCY
  deriving (Bounded, Enum, Eq, Ord, Read, Show, Typeable)

data PriorityLog l = PriorityLog {-# UNPACK #-} !Priority
                                 {-# UNPACK #-} !l

logTo :: (Typeable l, Member (Log (PriorityLog l)) r)
  => Priority -> l -> Eff r ()
logTo p l = logE (PriorityLog p l)
{-# INLINE logTo #-}

logDebug :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logDebug = logTo DEBUG

logInfo :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logInfo = logTo INFO

logNotice :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logNotice = logTo NOTICE

logWarning :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logWarning = logTo WARNING

logError :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logError = logTo ERROR

logCritical :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logCritical = logTo CRITICAL

logAlert :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logAlert = logTo ALERT

logEmergency :: (Typeable l, Member (Log (PriorityLog l)) r) => l -> Eff r ()
logEmergency = logTo EMERGENCY
