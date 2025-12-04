{-# LANGUAGE OverloadedStrings #-}

module Control.Carrier.Logger.WriterSpec (
  spec,
) where

import Control.Carrier.Interpret (run)
import Control.Carrier.Logger.Writer (runLoggerW)
import Control.Monad.Logger (LogLevel (..), defaultLoc, logDebugN, logInfoN, logWarnN)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "LoggerWriterC" $ do
    it "no log" $
      run (runLoggerW (pure ())) `shouldBe` ([], ())

    it "3 logs" $ do
      let action = do
            logDebugN "1"
            logInfoN "2"
            logWarnN "3"
          logLines =
            [ (defaultLoc, "", LevelDebug, "1")
            , (defaultLoc, "", LevelInfo, "2")
            , (defaultLoc, "", LevelWarn, "3")
            ]

      run (runLoggerW action) `shouldBe` (logLines, ())
