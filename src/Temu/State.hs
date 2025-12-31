{-# LANGUAGE OverloadedStrings #-}

-- | Application state management using STM
module Temu.State
  ( -- * State types
    AppState (..),
    AppStateVar,

    -- * State creation
    newAppState,
    initialState,

    -- * State access (IO wrappers around STM)
    readAppState,
    modifyAppState,
    modifyAppState_,

    -- * STM transactions (for composition)
    readAppStateSTM,
    modifyAppStateSTM,

    -- * Specific state operations
    updateCursorBlink,
    appendToInput,
    deleteLastChar,
    clearInputBuffer,
    setOutputLines,
  )
where

import Control.Concurrent.STM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Temu.Config (welcomeMessage)

-- | The main application state
data AppState = AppState
  { -- | Current input line being typed
    inputBuffer :: !Text,
    -- | Lines of output to display
    outputLines :: ![Text],
    -- | Whether the cursor is currently visible (for blinking)
    cursorVisible :: !Bool,
    -- | Timestamp of last cursor blink toggle
    lastBlinkTime :: !Word32
  }
  deriving (Show, Eq)

-- | TVar holding the application state
type AppStateVar = TVar AppState

-- | Initial application state
initialState :: AppState
initialState =
  AppState
    { inputBuffer = "",
      outputLines = welcomeMessage,
      cursorVisible = True,
      lastBlinkTime = 0
    }

-- | Create a new state variable with initial state
newAppState :: IO AppStateVar
newAppState = newTVarIO initialState

-- | Read the current state (IO wrapper)
readAppState :: AppStateVar -> IO AppState
readAppState = readTVarIO

-- | Modify the state with a function (IO wrapper, returns new state)
modifyAppState :: AppStateVar -> (AppState -> AppState) -> IO AppState
modifyAppState var f = atomically $ do
  modifyAppStateSTM var f
  readTVar var

-- | Modify the state with a function (IO wrapper, discards result)
modifyAppState_ :: AppStateVar -> (AppState -> AppState) -> IO ()
modifyAppState_ var f = atomically $ modifyAppStateSTM var f

-- | Read state in STM (for composing transactions)
readAppStateSTM :: AppStateVar -> STM AppState
readAppStateSTM = readTVar

-- | Modify state in STM (for composing transactions)
modifyAppStateSTM :: AppStateVar -> (AppState -> AppState) -> STM ()
modifyAppStateSTM = modifyTVar'

-- | Update cursor blink state if enough time has passed
updateCursorBlink :: Word32 -> Word32 -> AppState -> AppState
updateCursorBlink currentTime blinkInterval state
  | currentTime - lastBlinkTime state > blinkInterval =
      state
        { cursorVisible = not (cursorVisible state),
          lastBlinkTime = currentTime
        }
  | otherwise = state

-- | Append text to the input buffer
appendToInput :: Text -> AppState -> AppState
appendToInput txt state = state {inputBuffer = inputBuffer state <> txt}

-- | Delete the last character from the input buffer
deleteLastChar :: AppState -> AppState
deleteLastChar state =
  state
    { inputBuffer =
        if T.null (inputBuffer state)
          then ""
          else T.init (inputBuffer state)
    }

-- | Clear the input buffer
clearInputBuffer :: AppState -> AppState
clearInputBuffer state = state {inputBuffer = ""}

-- | Set the output lines
setOutputLines :: [Text] -> AppState -> AppState
setOutputLines newLines state = state {outputLines = newLines}
