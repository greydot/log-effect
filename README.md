# Log effect

[![Build Status](https://secure.travis-ci.org/greydot/log-effect.png)](http://travis-ci.org/greydot/log-effect)

An extensible log effect using extensible-effects. This library introduces two
new effects to your extensible effects arsenal, `Log` and `LogM`.

## Log

This is the simpler of the two. `Log` allows for logging in pure code, as well
as filtering using `filterLog`. The downside of this effect is that when your
code launches multiple threads using `async` or `forkIO`, messages from every
thread other than the main thread will be lost.

## LogM

`LogM` loses the ability to provide logging in pure code, but at the same time
allows to log messages from multiple threads.

## Example

```haskell
import Control.Concurrent.Lifted
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Log

someComp :: ( [ Log String, LogM IO String ] <:: r
            , Lifted IO r
            ) => Eff r ()
someComp = do logE "Hello!"
              logM "Greetings from the main thread!"
              
              fork $ do logM "This is a new thread, and this message is still visible."
                        logE "Unfortunately, this one is not."

main :: IO ()
main = runLift $ runLog stdoutLogger $ runLogM stdoutLogger $ someComp
```
