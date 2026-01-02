{-# LANGUAGE OverloadedStrings #-}

module Temu.InputSpec (spec) where

import qualified SDL
import Temu.Input
import Test.Hspec

spec :: Spec
spec = do
  describe "InputAction" $ do
    it "NoAction shows correctly" $ do
      show NoAction `shouldBe` "NoAction"

    it "Quit shows correctly" $ do
      show Quit `shouldBe` "Quit"

    it "SendBytes shows correctly" $ do
      show (SendBytes "test") `shouldBe` "SendBytes \"test\""

  describe "keyToBytes" $ do
    it "converts Return to carriage return" $ do
      keyToBytes SDL.KeycodeReturn `shouldBe` Just "\r"

    it "converts Backspace to DEL" $ do
      keyToBytes SDL.KeycodeBackspace `shouldBe` Just "\x7f"

    it "converts Tab correctly" $ do
      keyToBytes SDL.KeycodeTab `shouldBe` Just "\t"

    it "converts Escape correctly" $ do
      keyToBytes SDL.KeycodeEscape `shouldBe` Just "\x1b"

    it "converts arrow keys to VT100 sequences" $ do
      keyToBytes SDL.KeycodeUp `shouldBe` Just "\x1b[A"
      keyToBytes SDL.KeycodeDown `shouldBe` Just "\x1b[B"
      keyToBytes SDL.KeycodeRight `shouldBe` Just "\x1b[C"
      keyToBytes SDL.KeycodeLeft `shouldBe` Just "\x1b[D"

    it "converts Home/End correctly" $ do
      keyToBytes SDL.KeycodeHome `shouldBe` Just "\x1b[H"
      keyToBytes SDL.KeycodeEnd `shouldBe` Just "\x1b[F"

    it "returns Nothing for unmapped keys" $ do
      keyToBytes SDL.KeycodeA `shouldBe` Nothing
      keyToBytes SDL.KeycodeSpace `shouldBe` Nothing
