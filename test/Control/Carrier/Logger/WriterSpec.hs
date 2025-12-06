{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Carrier.Logger.WriterSpec (
  spec,
) where

import Control.Carrier.Interpret (run)
import Control.Carrier.Logger.Writer (runLoggerW)
import Control.Monad (forM_)
import Control.Monad.Logger (
  Loc (..),
  LogLevel (..),
  LogLine,
  defaultLoc,
  logDebugN,
  logDebugS,
  logInfoN,
  logInfoS,
  logWarnN,
  logWarnS,
 )
import System.FilePath ((<.>), (</>))
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldContain, shouldNotBe)

spec :: Spec
spec = do
  describe "LoggerWriterC" $ do
    context "non-TH" $ do
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

    it "TH" $ do
      let action =
            do
              $logDebugS "test" "1"
              $logInfoS "test" "2"
              $logWarnS "test" "3"
          expected =
            [ (LevelDebug, "1")
            , (LevelInfo, "2")
            , (LevelWarn, "3")
            ]
          results = run (runLoggerW action)

      length (fst results) `shouldBe` 3
      forM_ (zip (fst results :: [LogLine]) expected) $
        \((loc, src, lvl, msg), (expLvl, expMsg)) -> do
          loc_filename loc
            `shouldBe` "test" </> "Control" </> "Carrier" </> "Logger" </> "WriterSpec" <.> "hs"
          loc_package loc `shouldContain` "fused-effects-logger"
          loc_module loc `shouldBe` "Control.Carrier.Logger.WriterSpec"
          loc_start loc `shouldNotBe` (0, 0)
          loc_end loc `shouldNotBe` (0, 0)
          src `shouldBe` "test"
          lvl `shouldBe` expLvl
          msg `shouldBe` expMsg
