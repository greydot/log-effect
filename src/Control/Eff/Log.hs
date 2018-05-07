{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
module Control.Eff.Log ( Log
                       , LogM
                       , Logger
                       , stdoutLogger
                       , stderrLogger
                       , LogMessage(..)
                       , logE
                       , logM
                       , filterLog
                       , filterLog'
                       , runLogPure
                       , runLog
                       , runLogM
                       ) where

import Control.Applicative   ((<$>), (<*), (<$))
import Control.Eff
import Control.Eff.Lift      (Lifted, lift)
import Control.Monad         (when)
import Control.Monad.Base    (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable         (Typeable)
import System.IO (stderr, stdout)

-- | Simple log effect, useful in pure code
data Log l v where
  Log :: l -> Log l ()

instance ( MonadBase m m
         , Lifted m r
         , MonadBaseControl m (Eff r)
         ) => MonadBaseControl m (Eff (Log l ': r)) where
    type StM (Eff (Log l ': r)) a = StM (Eff r) (a, [l])
    liftBaseWith f = raise $ liftBaseWith $ \runInBase ->
                       f (runInBase . runLogPure)
    restoreM x = do (a, ls :: [l]) <- raise (restoreM x)
                    mapM_ logE ls
                    return a

logLine :: Log a v -> a
logLine (Log l) = l

-- | Monadic action that does the real logging
type Logger m l = l -> m ()

-- | Logger that outputs messages to stdout
stdoutLogger :: (LogMessage l, MonadBase IO m) => Logger m l
stdoutLogger = liftBase . Char8.hPutStrLn stdout . toMsg

-- | Logger that outputs messages to stderr
stderrLogger :: (LogMessage l, MonadBase IO m) => Logger m l
stderrLogger = liftBase . Char8.hPutStrLn stderr . toMsg

-- | Log something.
logE :: Member (Log l) r => l -> Eff r ()
logE = send . Log

-- | Collect log messages in a list.
runLogPure :: Eff (Log l ': r) a -> Eff r (a, [l])
runLogPure = handle_relay (\x -> return (x, []))
                          (\(Log l) k -> k () >>= \(x, ls) -> return (x, l:ls))

-- | Run the 'Logger' action in the base monad for every log line.
runLog :: Lifted m r => Logger m l -> Eff (Log l ': r) a -> Eff r a
runLog logger = handle_relay return
                             (\(Log l) k -> lift (logger l) >> k ())

-- | Filter Log entries with a predicate.
--
-- Note that, most of the time an explicit type signature for the predicate
-- will be required.
filterLog :: forall l r a. Member (Log l) r
          => (l -> Bool) -> Eff r a -> Eff r a
filterLog f = interpose return h
  where
    h :: Log l v -> (v -> Eff r b) -> Eff r b
    h (Log l) k = when (f l) (logE l) >> k ()


-- | Filter Log entries with a predicate and a proxy.
--
-- This is the same as 'filterLog' but with a proxy l for type inference.
filterLog' :: Member (Log l) r
           => (l -> Bool) -> proxy l -> Eff r a -> Eff r a
filterLog' predicate _ = filterLog predicate

-- | A more advanced version of 'Log'. Adds an ability to log from multiple threads.
data LogM m l v where
  AskLogger :: LogM m l (Logger m l)

askLogger :: Member (LogM m l) r => Eff r (Logger m l)
askLogger = send AskLogger

-- | Log something using `LogM` effect
logM :: (Member (LogM m l) r, Lifted m r) => l -> Eff r ()
logM l = do logger <- askLogger
            lift (logger l)

-- | Run the 'Logger' action in the base monad for every log line.
runLogM :: Lifted m r => Logger m l -> Eff (LogM m l ': r) a -> Eff r a
runLogM logger = handle_relay return
                              (\AskLogger -> ($ logger))

instance ( MonadBase m m
         , Lifted m r
         , MonadBaseControl m (Eff r)
         ) => MonadBaseControl m (Eff (LogM m l ': r)) where
    type StM (Eff (LogM m l ': r)) a = StM (Eff r) a
    liftBaseWith f = do l <- askLogger
                        raise $ liftBaseWith $ \runInBase ->
                          f (runInBase . runLogM l)
    restoreM = raise . restoreM

-- | Handy typeclass to convert log messages for output
class LogMessage l where
  toMsg :: l -> ByteString
  {-# MINIMAL toMsg #-}

instance LogMessage ByteString where
  toMsg = id

instance LogMessage [Char] where
  toMsg = Char8.pack

instance LogMessage Text where
  toMsg = encodeUtf8
