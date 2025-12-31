{-# LANGUAGE OverloadedStrings #-}

module Temu.StateSpec (spec) where

import Temu.State
    ( AppState(lastBlinkTime, outputLines, inputBuffer, cursorVisible),
      deleteLastChar,
      clearInputBuffer,
      appendToInput,
      initialState,
      modifyAppState_,
      newAppState,
      readAppState,
      updateCursorBlink )
import Test.Hspec ( describe, it, shouldBe, shouldSatisfy, Spec )

spec :: Spec
spec = do
  describe "AppState" $ do
    describe "initialState" $ do
      it "has empty input buffer" $ do
        inputBuffer initialState `shouldBe` ""

      it "has welcome message in output" $ do
        length (outputLines initialState) `shouldSatisfy` (> 0)

      it "has cursor visible" $ do
        cursorVisible initialState `shouldBe` True

  describe "STM operations" $ do
    describe "newAppState" $ do
      it "creates state with initial values" $ do
        stateVar <- newAppState
        state <- readAppState stateVar
        inputBuffer state `shouldBe` ""

    describe "modifyAppState_" $ do
      it "modifies the state" $ do
        stateVar <- newAppState
        modifyAppState_ stateVar (appendToInput "hello")
        state <- readAppState stateVar
        inputBuffer state `shouldBe` "hello"

  describe "Pure state operations" $ do
    describe "appendToInput" $ do
      it "appends text to empty buffer" $ do
        let state = initialState
            state' = appendToInput "hello" state
        inputBuffer state' `shouldBe` "hello"

      it "appends text to existing buffer" $ do
        let state = initialState {inputBuffer = "hello"}
            state' = appendToInput " world" state
        inputBuffer state' `shouldBe` "hello world"

    describe "deleteLastChar" $ do
      it "removes last character" $ do
        let state = initialState {inputBuffer = "hello"}
            state' = deleteLastChar state
        inputBuffer state' `shouldBe` "hell"

      it "handles empty buffer gracefully" $ do
        let state = initialState {inputBuffer = ""}
            state' = deleteLastChar state
        inputBuffer state' `shouldBe` ""

    describe "clearInputBuffer" $ do
      it "clears the buffer" $ do
        let state = initialState {inputBuffer = "hello"}
            state' = clearInputBuffer state
        inputBuffer state' `shouldBe` ""

    describe "updateCursorBlink" $ do
      it "toggles cursor when interval passed" $ do
        let state = initialState {cursorVisible = True, lastBlinkTime = 0}
            state' = updateCursorBlink 600 500 state
        cursorVisible state' `shouldBe` False
        lastBlinkTime state' `shouldBe` 600

      it "does not toggle cursor when interval not passed" $ do
        let state = initialState {cursorVisible = True, lastBlinkTime = 0}
            state' = updateCursorBlink 400 500 state
        cursorVisible state' `shouldBe` True
        lastBlinkTime state' `shouldBe` 0
