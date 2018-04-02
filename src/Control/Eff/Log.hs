{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Control.Eff.Log
  ( Log
  , Logger
  , logE
  , filterLog
  , filterLog'
  , runLogPure
  , runLogStdout
  , runLogStderr
  , runLogFile
  , runLogWithLoggerSet
--  , runLog
  -- | reexports from fast-logger
  , ToLogStr(toLogStr)
  , LogStr
  ) where

import Control.Applicative   ((<$>), (<*), (<$))
import Control.Eff
import Control.Eff.Lift      (Lifted, lift)
import Control.Monad         (when)
import Data.Monoid           ((<>))
import Data.Typeable         (Typeable)
import System.Log.FastLogger (LogStr, LoggerSet, ToLogStr, defaultBufSize,
                              flushLogStr, fromLogStr, newFileLoggerSet,
                              newStderrLoggerSet, newStdoutLoggerSet,
                              pushLogStr, toLogStr)

import qualified Data.ByteString.Char8 as B8

data Log a v where
  Log :: a -> Log a ()

logLine :: Log a v -> a
logLine (Log l) = l

-- | a monadic action that does the real logging
type Logger m l = l -> m ()

-- | Log something.
logE :: (Typeable l, Member (Log l) r) => l -> Eff r ()
logE = send . Log

-- | Collect log messages in a list.
runLogPure :: (Typeable l)
  => Eff (Log l ': r) a
  -> Eff r (a, [l])
runLogPure = handle_relay (\x -> return (x, []))
                          (\(Log l) k -> k () >>= \(x, ls) -> return (x, l:ls))

-- | Run the 'Logger' action in the base monad for every log line.
runLog :: (Typeable l, Typeable m, Lifted m r)
  => Logger m l -> Eff (Log l ': r) a -> Eff r a
runLog logger = handle_relay return
                             (\(Log l) k -> k () >>= \x -> x <$ lift (logger l))

-- | Filter Log entries with a predicate.
--
-- Note that, most of the time an explicit type signature for the predicate
-- will be required.
filterLog :: forall l r a. (Typeable l, Member (Log l) r)
  => (l -> Bool) -> Eff r a -> Eff r a
filterLog f = interpose return h
  where
    h :: Log l v -> (v -> Eff r b) -> Eff r b
    h (Log l) k = when (f l) (logE l) >> k ()


-- | Filter Log entries with a predicate and a proxy.
--
-- This is the same as 'filterLog' but with a proxy l for type inference.
filterLog' :: (Typeable l, Member (Log l) r)
  => (l -> Bool) -> proxy l -> Eff r a -> Eff r a
filterLog' predicate _ = filterLog predicate

-- | Log to stdout.
runLogStdout :: (Typeable l, ToLogStr l, Lifted IO r)
  => proxy l -> Eff (Log l ': r) a -> Eff r a
runLogStdout proxy eff = do
    s <- lift $ newStdoutLoggerSet defaultBufSize
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

-- | Log to stderr.
runLogStderr :: (Typeable l, ToLogStr l, Lifted IO r)
  => proxy l -> Eff (Log l ': r) a -> Eff r a
runLogStderr proxy eff = do
    s <- lift $ newStderrLoggerSet defaultBufSize
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

-- | Log to file.
runLogFile :: (Typeable l, ToLogStr l, Lifted IO r)
  => FilePath -> proxy l -> Eff (Log l ': r) a -> Eff r a
runLogFile f proxy eff = do
    s <- lift $ newFileLoggerSet defaultBufSize f
    runLogWithLoggerSet s proxy eff <* lift (flushLogStr s)

-- | Log to a file using a 'LoggerSet'.
--
-- Note, that you will still have to call 'flushLogStr' on the 'LoggerSet'
-- at one point.
--
-- With that function you can combine a logger in a surrounding IO action
-- with a logger in the 'Eff' effect.
--
-- >data Proxy a = Proxy
-- >
-- > main :: IO ()
-- > main = do
-- >     loggerSet <- newStderrLoggerSet defaultBufSize
-- >     pushLogStr loggerSet "logging from outer space^WIO\n"
-- >     runLift $ runLogWithLoggerSet loggerSet (Proxy :: Proxy String) $
-- >         logE ("logging from within Eff" :: String)
-- >     flushLogStr loggerSet
runLogWithLoggerSet :: (Typeable l, ToLogStr l, Lifted IO r)
  => LoggerSet -> proxy l -> Eff (Log l ': r) a -> Eff r a
runLogWithLoggerSet s _ = runLog (loggerSetLogger s)

loggerSetLogger :: ToLogStr l => LoggerSet -> Logger IO l
loggerSetLogger loggerSet = pushLogStr loggerSet . (<> "\n") . toLogStr
