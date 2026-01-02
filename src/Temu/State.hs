{-# LANGUAGE OverloadedStrings #-}

-- | Application state management using STM
module Temu.State
  ( -- * State types
    AppState (..),
    AppStateVar,
    TerminalState (..),

    -- * Re-exports from VTerm
    Cell (..),
    Color (..),

    -- * State creation
    newAppState,
    newTerminalState,

    -- * State access (IO wrappers around STM)
    readAppState,
    modifyAppState,
    modifyAppState_,

    -- * STM transactions (for composition)
    readAppStateSTM,
    modifyAppStateSTM,

    -- * Terminal state operations
    updateCellGrid,
    updateCursorPos,

    -- * UI state operations
    updateCursorBlink,
  )
where

import Control.Concurrent.STM
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)
import Temu.PTY (PTY)
import Temu.VTerm (Cell (..), Color (..), VTerm)

-- | Terminal emulation state
data TerminalState = TerminalState
  { -- | libvterm handle
    termVTerm :: !VTerm,
    -- | PTY handle for shell communication
    termPTY :: !PTY,
    -- | Cached cell grid for rendering (row-major)
    termCellGrid :: !(Vector (Vector Cell)),
    -- | Current cursor position (row, col)
    termCursorPos :: !(Int, Int),
    -- | Terminal dimensions (rows, cols)
    termSize :: !(Int, Int)
  }

-- | The main application state
data AppState = AppState
  { -- | Terminal emulation state (lazy for testability)
    terminal :: TerminalState,
    -- | Whether the cursor is currently visible (for blinking)
    cursorVisible :: !Bool,
    -- | Timestamp of last cursor blink toggle
    lastBlinkTime :: !Word32
  }

-- | TVar holding the application state
type AppStateVar = TVar AppState

-- | Create an empty cell grid of given dimensions
emptyGrid :: Int -> Int -> Vector (Vector Cell)
emptyGrid rows cols = V.replicate rows (V.replicate cols emptyCell)
  where
    emptyCell = Cell ' ' (Color 255 255 255) (Color 0 0 0) False False False False

-- | Create a new terminal state (without STM wrapper)
newTerminalState :: VTerm -> PTY -> Int -> Int -> TerminalState
newTerminalState vt pty rows cols =
  TerminalState
    { termVTerm = vt,
      termPTY = pty,
      termCellGrid = emptyGrid rows cols,
      termCursorPos = (0, 0),
      termSize = (rows, cols)
    }

-- | Create a new state variable
newAppState :: TerminalState -> IO AppStateVar
newAppState termState =
  newTVarIO
    AppState
      { terminal = termState,
        cursorVisible = True,
        lastBlinkTime = 0
      }

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

-- | Update the cell grid in terminal state
updateCellGrid :: Vector (Vector Cell) -> AppState -> AppState
updateCellGrid grid state =
  state {terminal = (terminal state) {termCellGrid = grid}}

-- | Update the cursor position in terminal state
updateCursorPos :: (Int, Int) -> AppState -> AppState
updateCursorPos pos state =
  state {terminal = (terminal state) {termCursorPos = pos}}

-- | Update cursor blink state if enough time has passed
updateCursorBlink :: Word32 -> Word32 -> AppState -> AppState
updateCursorBlink currentTime blinkInterval state
  | currentTime - lastBlinkTime state > blinkInterval =
      state
        { cursorVisible = not (cursorVisible state),
          lastBlinkTime = currentTime
        }
  | otherwise = state
