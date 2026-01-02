{-# LANGUAGE OverloadedStrings #-}

module Temu.StateSpec (spec) where

import Temu.State
  ( AppState (..),
    Cell (..),
    Color (..),
    updateCursorBlink,
  )
import Test.Hspec

-- | Create a test-only AppState
-- Note: In real code, this would come from newAppState with real VTerm/PTY
testAppState :: AppState
testAppState =
  AppState
    { terminal = error "terminal not available in tests",
      cursorVisible = True,
      lastBlinkTime = 0
    }

spec :: Spec
spec = do
  describe "Cell" $ do
    it "can be created with default values" $ do
      let cell = Cell 'A' (Color 255 0 0) (Color 0 0 0) True False False False
      cellChar cell `shouldBe` 'A'
      cellBold cell `shouldBe` True

    it "has correct color components" $ do
      let color = Color 100 150 200
      colorR color `shouldBe` 100
      colorG color `shouldBe` 150
      colorB color `shouldBe` 200

  describe "Color" $ do
    it "can represent white" $ do
      let white = Color 255 255 255
      colorR white `shouldBe` 255
      colorG white `shouldBe` 255
      colorB white `shouldBe` 255

    it "can represent black" $ do
      let black = Color 0 0 0
      colorR black `shouldBe` 0
      colorG black `shouldBe` 0
      colorB black `shouldBe` 0

  describe "updateCursorBlink" $ do
    it "toggles cursor when interval passed" $ do
      let state = testAppState {cursorVisible = True, lastBlinkTime = 0}
          state' = updateCursorBlink 600 500 state
      cursorVisible state' `shouldBe` False
      lastBlinkTime state' `shouldBe` 600

    it "does not toggle cursor when interval not passed" $ do
      let state = testAppState {cursorVisible = True, lastBlinkTime = 0}
          state' = updateCursorBlink 400 500 state
      cursorVisible state' `shouldBe` True
      lastBlinkTime state' `shouldBe` 0

    it "toggles from invisible to visible" $ do
      let state = testAppState {cursorVisible = False, lastBlinkTime = 100}
          state' = updateCursorBlink 700 500 state
      cursorVisible state' `shouldBe` True
      lastBlinkTime state' `shouldBe` 700
