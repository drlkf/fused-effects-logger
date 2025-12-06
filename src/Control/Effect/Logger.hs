{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A simple logger effect that takes all the information a 'MonadLogger' monad
-- would take. Use the logging functions of 'MonadLogger' to log things the way
-- you want, as there are many options there (TH or non-TH).
module Control.Effect.Logger (
  Logger (..),
) where

import Control.Effect.Labelled (Has, send)
import Control.Monad.Logger (
  Loc,
  LogLevel,
  LogSource,
  MonadLogger (..),
  ToLogStr (..),
 )
import Data.Kind (Type)

data Logger (m :: Type -> Type) k where
  -- | See 'monadLoggerLog'.
  LoggerLog
    :: ToLogStr msg
    => Loc
    -> LogSource
    -> LogLevel
    -> msg
    -> Logger m ()

-- | Unfortunately, we need to implement the 'MonadLogger' instance as an orphan
-- in order to be able to use the same interface as 'MonadLogger'.
instance (Has Logger sig m, Monad m) => MonadLogger m where
  monadLoggerLog loc src lvl msg = send (LoggerLog loc src lvl msg)
