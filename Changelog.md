# Changelog

## Version 1.0

	* Complete revamp of the logging effects.

	* Add LogM effect.

	* Add MonadBaseControl instances.

	* Remove fast-logger dependency in favor of a custom typeclass.



## Version 0.4

 * add a proxy to most `runLog` functions to not have to supply the full
   specialized type signature. Use something like `data Proxy a = Proxy` or
   the one from `tagged`.

 * remove `ShowLog` typeclass in favor of `ToLogString` from `fast-logger`.
