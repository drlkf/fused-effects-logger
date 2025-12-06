{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An IO carrier for the 'Logger' effect that writes logs to a 'Handle'
-- selected by the provided function.
module Control.Carrier.Logger.IO (
  LoggerIOC (LoggerIOC),

  -- * Helpers
  HandleSelector,
  singleHandle,

  -- * Carrier implementation
  runLoggerIO,
  runStdoutLoggerIO,
  runStderrLoggerIO,
) where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Reader (ReaderC, runReader)
import Control.Effect.Logger (Logger (..))
import Control.Effect.Reader (ask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (
  Loc,
  LogLevel,
  LogSource,
  LogStr,
  ToLogStr (..),
  defaultLogStr,
  fromLogStr,
 )
import qualified Data.ByteString.Char8 as B (hPutStr)
import System.IO (Handle, stderr, stdout)

-- | Algebra for a simple IO logger. The inner function allows for selecting a
-- 'Handle' according to something like 'LogLevel'.
newtype LoggerIOC f m a = LoggerIOC
  { runLoggerIOC :: ReaderC f m a
  }
  deriving (Applicative, Functor, Monad, MonadIO)

type HandleSelector = Loc -> LogSource -> LogLevel -> LogStr -> Handle

-- | Helper to run IO loggers by writing to the same 'Handle' every time.
singleHandle
  :: Handle
  -> HandleSelector
singleHandle h _ _ _ _ = h

instance
  forall sig m
   . ( Algebra sig m
     , MonadIO m
     )
  => Algebra (Logger :+: sig) (LoggerIOC HandleSelector m)
  where
  alg hdl sig ctx = case sig of
    L (LoggerLog loc src lvl msg) -> LoggerIOC $ do
      f <- ask
      ctx
        <$ liftIO
          ( B.hPutStr
              (f loc src lvl (toLogStr msg))
              (fromLogStr (defaultLogStr loc src lvl (toLogStr msg)))
          )
    R other -> LoggerIOC (alg (runLoggerIOC . hdl) (R other) ctx)

-- | Run logger, writing to a 'Handle' selected by a function @f@.
runLoggerIO
  :: HandleSelector
  -- ^ @f@
  -> LoggerIOC HandleSelector m a
  -> m a
runLoggerIO f = runReader f . runLoggerIOC

-- | Run logger, writing everything to 'stdout'.
runStdoutLoggerIO
  :: LoggerIOC HandleSelector m a
  -> m a
runStdoutLoggerIO = runLoggerIO (singleHandle stdout)

-- | Run logger, writing everything to 'stderr'.
runStderrLoggerIO
  :: LoggerIOC HandleSelector m a
  -> m a
runStderrLoggerIO = runLoggerIO (singleHandle stderr)
