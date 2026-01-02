{-# LANGUAGE OverloadedStrings #-}

-- | Input handling for terminal emulator
module Temu.Input
  ( -- * Input actions (pure, testable)
    InputAction (..),
    classifyEvent,

    -- * Key to byte conversion
    keyToBytes,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text.Encoding (encodeUtf8)
import qualified SDL

-- | Actions that can result from user input
data InputAction
  = -- | No action needed
    NoAction
  | -- | Send bytes to PTY (text input or special key)
    SendBytes !ByteString
  | -- | User requested to quit
    Quit
  | -- | Mouse button pressed at pixel position (x, y)
    MouseDown !Int32 !Int32
  | -- | Mouse moved while button held at pixel position (x, y)
    MouseDrag !Int32 !Int32
  | -- | Mouse button released at pixel position (x, y)
    MouseUp !Int32 !Int32
  | -- | Copy selection to clipboard (Cmd+C)
    Copy
  | -- | Paste from clipboard (Cmd+V)
    Paste
  deriving (Show, Eq)

-- | Convert a special key to terminal escape sequence
keyToBytes :: SDL.Keycode -> Maybe ByteString
keyToBytes keycode = case keycode of
  SDL.KeycodeReturn -> Just "\r"
  SDL.KeycodeBackspace -> Just "\x7f" -- DEL character
  SDL.KeycodeTab -> Just "\t"
  SDL.KeycodeEscape -> Just "\x1b"
  -- Arrow keys (VT100 sequences)
  SDL.KeycodeUp -> Just "\x1b[A"
  SDL.KeycodeDown -> Just "\x1b[B"
  SDL.KeycodeRight -> Just "\x1b[C"
  SDL.KeycodeLeft -> Just "\x1b[D"
  -- Home/End
  SDL.KeycodeHome -> Just "\x1b[H"
  SDL.KeycodeEnd -> Just "\x1b[F"
  -- Page Up/Down
  SDL.KeycodePageUp -> Just "\x1b[5~"
  SDL.KeycodePageDown -> Just "\x1b[6~"
  -- Insert/Delete
  SDL.KeycodeInsert -> Just "\x1b[2~"
  SDL.KeycodeDelete -> Just "\x1b[3~"
  -- Function keys
  SDL.KeycodeF1 -> Just "\x1bOP"
  SDL.KeycodeF2 -> Just "\x1bOQ"
  SDL.KeycodeF3 -> Just "\x1bOR"
  SDL.KeycodeF4 -> Just "\x1bOS"
  SDL.KeycodeF5 -> Just "\x1b[15~"
  SDL.KeycodeF6 -> Just "\x1b[17~"
  SDL.KeycodeF7 -> Just "\x1b[18~"
  SDL.KeycodeF8 -> Just "\x1b[19~"
  SDL.KeycodeF9 -> Just "\x1b[20~"
  SDL.KeycodeF10 -> Just "\x1b[21~"
  SDL.KeycodeF11 -> Just "\x1b[23~"
  SDL.KeycodeF12 -> Just "\x1b[24~"
  _ -> Nothing

-- | Classify any SDL event into an input action
classifyEvent :: SDL.EventPayload -> InputAction
classifyEvent payload = case payload of
  SDL.QuitEvent -> Quit
  SDL.KeyboardEvent keyEvent
    | SDL.keyboardEventKeyMotion keyEvent == SDL.Pressed ->
        let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyEvent)
            mods = SDL.keysymModifier (SDL.keyboardEventKeysym keyEvent)
            hasCmd = SDL.keyModifierLeftGUI mods || SDL.keyModifierRightGUI mods
            hasCtrl = SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods
         in -- Handle Cmd+C/V for clipboard (macOS), Ctrl+C/D/etc for terminal signals
            if hasCmd
              then case keycode of
                SDL.KeycodeC -> Copy
                SDL.KeycodeV -> Paste
                _ -> NoAction
              else
                if hasCtrl
                  then case keycode of
                    SDL.KeycodeC -> SendBytes "\x03" -- SIGINT
                    SDL.KeycodeD -> SendBytes "\x04" -- EOF
                    SDL.KeycodeZ -> SendBytes "\x1a" -- SIGTSTP
                    SDL.KeycodeL -> SendBytes "\x0c" -- Clear screen
                    SDL.KeycodeU -> SendBytes "\x15" -- Kill line
                    SDL.KeycodeW -> SendBytes "\x17" -- Kill word
                    _ -> maybe NoAction SendBytes (keyToBytes keycode)
                  else maybe NoAction SendBytes (keyToBytes keycode)
  SDL.KeyboardEvent _ -> NoAction
  SDL.TextInputEvent textEvent ->
    let txt = SDL.textInputEventText textEvent
     in SendBytes (encodeUtf8 txt)
  -- Mouse button pressed (left button only)
  SDL.MouseButtonEvent mouseEvent
    | SDL.mouseButtonEventButton mouseEvent == SDL.ButtonLeft ->
        let SDL.P (SDL.V2 x y) = SDL.mouseButtonEventPos mouseEvent
         in case SDL.mouseButtonEventMotion mouseEvent of
              SDL.Pressed -> MouseDown x y
              SDL.Released -> MouseUp x y
  SDL.MouseButtonEvent _ -> NoAction
  -- Mouse motion while button held (for selection dragging)
  SDL.MouseMotionEvent motionEvent
    | not (null (SDL.mouseMotionEventState motionEvent)) ->
        -- Button is being held
        let SDL.P (SDL.V2 x y) = SDL.mouseMotionEventPos motionEvent
         in MouseDrag x y
  SDL.MouseMotionEvent _ -> NoAction
  _ -> NoAction
