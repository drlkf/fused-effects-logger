# fused-effects-logger

A logger effect for the
[`fused-effects`](https://github.com/fused-effects/fused-effects) library, based
on the [`monad-logger`](https://github.com/snoyberg/monad-logger) API.

# Usage
In addition to `fused-effects-logger`, you most probably want to add
`monad-logger` as well in order to use its logging functions.

Here is a simple example to start running a program that logs to `stderr`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Carrier.Lift (runM)
import Control.Carrier.Logger.IO (runStderrLoggerIO)
import Control.Effect.Lift (sendM)
import Control.Monad.Logger (logInfo)

main :: IO ()
main = runM . runStderrLoggerIO $ do
  $(logInfo) "hello from fused-effects-logger"
  sendM (print 42)
```

It should produce the output:

```
[Info] hello from fused-effects-logger @(test-logger-0.1.0.0-KTuyx50rxPBAwC1EHp8UuV-test-logger-exe:Main app/Main.hs:13:4)
42
```

# Contributing

Please follow the process describe in [`RELEASE.md`](./RELEASE.md) to release
new versions to Hackage.
