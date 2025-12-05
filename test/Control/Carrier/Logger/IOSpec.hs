{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Control.Carrier.Logger.IOSpec (
  spec,
) where

import Control.Exception (try, IOException)
import Control.Carrier.Lift (runM)
import Control.Carrier.Logger.IO (runLoggerIO, singleHandle)
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN)
import System.Directory (removePathForcibly)
import System.IO (hClose, openTempFile)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  let runTestLogger action = do
        (fp, h) <- openTempFile "." "LoggerIOC.log"
        _ <- runM (runLoggerIO (singleHandle h) action)
        hClose h
        readFile fp <* (try @IOException (removePathForcibly fp))

  describe "LoggerIOC" $ do
    it "no log" $ do
      runTestLogger (pure ()) `shouldReturn` ""

    it "3 logs" $ do
      let action = do
            logDebugN "1"
            logInfoN "2"
            logWarnN "3"
          logLines =
            [ "[Debug] 1"
            , "[Info] 2"
            , "[Warn] 3"
            ]

      lines <$> runTestLogger action `shouldReturn` logLines
