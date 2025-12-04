{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

--
-- Effect definition
--

-- | A simple logger effect that takes all information a 'MonadLogger' monad
-- would take. In order to be able to use the same interface as 'MonadLogger',
-- that unfortunately means it needs to implement the 'MonadLogger' instance as
-- an orphan.
data Logger (m :: Type -> Type) k where
  LoggerLog
    :: ToLogStr msg
    => Loc
    -> LogSource
    -> LogLevel
    -> msg
    -> Logger m ()

-- ORPHAN :(
instance (Has Logger sig m, Monad m) => MonadLogger m where
  monadLoggerLog loc src lvl msg = send (LoggerLog loc src lvl msg)
