{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Carrier.Logger.Writer (
  LoggerWriterC (..),
  runLoggerW,
) where

import Control.Algebra (Algebra (..), (:+:) (..))
import Control.Carrier.Writer.Strict (WriterC, runWriter, tell)
import Control.Effect.Logger (Logger (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (
  LogLine,
  ToLogStr (..),
 )

-- | Reinterpreter from logger to writer. The underlying 'Writer' is always
-- 'Control.Carrier.Writer.Strict'.
newtype LoggerWriterC w m a = LoggerWriterC
  { runLoggerWriterC :: WriterC (w LogLine) m a
  }
  deriving (Applicative, Functor, Monad, MonadIO)

instance
  forall sig w m
   . ( Algebra sig m
     , Applicative w
     , Monoid (w LogLine)
     )
  => Algebra (Logger :+: sig) (LoggerWriterC w m)
  where
  alg hdl sig ctx = case sig of
    L (LoggerLog loc src lvl msg) ->
      ctx <$ LoggerWriterC (tell (pure (loc, src, lvl, toLogStr msg) :: w LogLine))
    R other ->
      LoggerWriterC (alg (runLoggerWriterC . hdl) (R other) ctx)

runLoggerW
  :: Monoid (w LogLine)
  => LoggerWriterC w m a
  -> m (w LogLine, a)
runLoggerW = runWriter . runLoggerWriterC
