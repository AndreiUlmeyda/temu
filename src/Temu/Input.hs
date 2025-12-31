{-# LANGUAGE OverloadedStrings #-}

-- | Input handling with pure event classification
module Temu.Input
  ( -- * Input actions (pure, testable)
    InputAction (..),
    classifyKeyEvent,
    classifyEvent,

    -- * State application (pure)
    applyInputAction,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified SDL
import Temu.State (AppState (..), appendToInput, clearInputBuffer, deleteLastChar)

-- | Actions that can result from user input
data InputAction
  = -- | No action needed
    NoAction
  | -- | User typed text
    TypeChar !Text
  | -- | User pressed backspace
    DeleteChar
  | -- | User pressed Enter with a command to execute
    SubmitCommand !Text
  | -- | User requested to quit
    Quit
  deriving (Show, Eq)

-- | Classify a keyboard event into an action
-- This is a pure function, easily testable!
classifyKeyEvent :: SDL.KeyboardEventData -> AppState -> InputAction
classifyKeyEvent keyEvent state
  | SDL.keyboardEventKeyMotion keyEvent /= SDL.Pressed = NoAction
  | otherwise = case SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent) of
      SDL.KeycodeReturn ->
        let cmd = inputBuffer state
         in if T.null cmd then NoAction else SubmitCommand cmd
      SDL.KeycodeBackspace -> DeleteChar
      SDL.KeycodeEscape -> NoAction -- Could be Quit
      _ -> NoAction

-- | Classify any SDL event into an input action
classifyEvent :: SDL.EventPayload -> AppState -> InputAction
classifyEvent payload state = case payload of
  SDL.QuitEvent -> Quit
  SDL.KeyboardEvent keyEvent -> classifyKeyEvent keyEvent state
  SDL.TextInputEvent textEvent -> TypeChar (SDL.textInputEventText textEvent)
  _ -> NoAction

-- | Apply an input action to the state (pure function)
-- Note: SubmitCommand only clears the buffer; actual execution happens in App
applyInputAction :: InputAction -> AppState -> AppState
applyInputAction action state = case action of
  NoAction -> state
  TypeChar txt -> appendToInput txt state
  DeleteChar -> deleteLastChar state
  SubmitCommand _ -> clearInputBuffer state
  Quit -> state -- Quit doesn't modify state
