{-# LANGUAGE OverloadedStrings #-}

module Temu.InputSpec (spec) where

import Temu.Input
import Temu.State (AppState (..), initialState)
import Test.Hspec

spec :: Spec
spec = do
  describe "InputAction" $ do
    describe "applyInputAction" $ do
      it "NoAction leaves state unchanged" $ do
        let state = initialState
            state' = applyInputAction NoAction state
        state' `shouldBe` state

      it "TypeChar appends to input buffer" $ do
        let state = initialState
            state' = applyInputAction (TypeChar "a") state
        inputBuffer state' `shouldBe` "a"

      it "TypeChar accumulates characters" $ do
        let state = initialState {inputBuffer = "hel"}
            state' = applyInputAction (TypeChar "lo") state
        inputBuffer state' `shouldBe` "hello"

      it "DeleteChar removes last character" $ do
        let state = initialState {inputBuffer = "hello"}
            state' = applyInputAction DeleteChar state
        inputBuffer state' `shouldBe` "hell"

      it "DeleteChar on empty buffer stays empty" $ do
        let state = initialState {inputBuffer = ""}
            state' = applyInputAction DeleteChar state
        inputBuffer state' `shouldBe` ""

      it "SubmitCommand clears input buffer" $ do
        let state = initialState {inputBuffer = "ls -la"}
            state' = applyInputAction (SubmitCommand "ls -la") state
        inputBuffer state' `shouldBe` ""

      it "Quit leaves state unchanged" $ do
        let state = initialState {inputBuffer = "test"}
            state' = applyInputAction Quit state
        state' `shouldBe` state

  describe "InputAction Show instance" $ do
    it "can show all action types" $ do
      show NoAction `shouldBe` "NoAction"
      show (TypeChar "x") `shouldBe` "TypeChar \"x\""
      show DeleteChar `shouldBe` "DeleteChar"
      show (SubmitCommand "ls") `shouldBe` "SubmitCommand \"ls\""
      show Quit `shouldBe` "Quit"
