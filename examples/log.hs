{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Concurrent
import Control.Eff
import Control.Eff.Log
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseDiscard)

-- This function is declared in lifted-base package.
fork :: MonadBaseControl IO m => m () -> m ThreadId
fork = liftBaseDiscard forkIO

someComp :: ( [ Log String, LogM IO String ] <:: r
            , LiftedBase IO r
            ) => Eff r ()
someComp = do logE "Hello!"
              logM "Greetings from the main thread!"

              _ <- fork $ do logM "This is a new thread, and this message is still visible."
                             logE "Unfortunately, this one is not."
              return ()

main :: IO ()
main = runLift $ runLog l $ runLogM l $ someComp
  where
    l = stdoutLogger :: Logger IO String
