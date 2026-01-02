{-# LANGUAGE OverloadedStrings #-}

-- | Main application module - wires everything together
module Temu.App
  ( -- * Application entry point
    run,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Raw.Types as Raw
import Temu.Config
  ( appTitle,
    cursorBlinkMs,
    fontPath,
    fontSize,
    frameDelayMs,
  )
import Temu.Input (InputAction (..), classifyEvent)
import Temu.PTY (PTY, closePTY, readPTY, spawnShell, writePTY)
import Temu.Render (render)
import Temu.State
  ( AppStateVar,
    Cell (..),
    modifyAppState_,
    newAppState,
    newTerminalState,
    readAppState,
    updateCellGrid,
    updateCursorBlink,
    updateCursorPos,
  )
import Temu.VTerm
  ( VTerm,
    freeVTerm,
    getCell,
    getCursorPos,
    getScreen,
    getSize,
    inputWrite,
    newVTerm,
  )

-- | Terminal dimensions
termRows, termCols :: Int
termRows = 24
termCols = 80

-- | Read the cell grid from VTerm
readCellGrid :: VTerm -> IO (Vector (Vector Cell))
readCellGrid vterm = do
  screen <- getScreen vterm
  (rows, cols) <- getSize vterm
  V.generateM rows $ \row ->
    V.generateM cols $ \col ->
      getCell screen row col

-- | PTY reader thread - reads from PTY and feeds to VTerm
ptyReaderThread :: PTY -> VTerm -> TVar Bool -> IO ()
ptyReaderThread pty vterm running = go
  where
    go = do
      isRunning <- readTVarIO running
      when isRunning $ do
        bytes <- readPTY pty
        unless (BS.null bytes) $ do
          _ <- inputWrite vterm bytes
          return ()
        threadDelay 1000 -- 1ms delay
        go

-- | Update the state with current VTerm screen contents
syncVTermToState :: VTerm -> AppStateVar -> IO ()
syncVTermToState vterm stateVar = do
  grid <- readCellGrid vterm
  cursorPos <- getCursorPos vterm
  modifyAppState_ stateVar $ \s ->
    updateCursorPos cursorPos $ updateCellGrid grid s

-- | Process a single event and return whether to quit
processEvent :: SDL.Event -> PTY -> IO Bool
processEvent event pty = do
  let action = classifyEvent (SDL.eventPayload event)
  case action of
    Quit -> return True
    SendBytes bytes -> do
      writePTY pty bytes
      return False
    NoAction -> return False

-- | Main application loop
appLoop :: SDL.Renderer -> TTF.Font -> AppStateVar -> PTY -> VTerm -> IO ()
appLoop renderer font stateVar pty vterm = do
  -- Poll and process events
  events <- SDL.pollEvents
  quitSignals <- mapM (\e -> processEvent e pty) events
  let quit = or quitSignals

  -- Sync VTerm state to our state
  syncVTermToState vterm stateVar

  -- Update cursor blink
  currentTime <- SDL.ticks
  modifyAppState_ stateVar (updateCursorBlink currentTime (fromIntegral cursorBlinkMs))

  -- Render current state
  state <- readAppState stateVar
  render renderer font state

  -- Frame delay (~60 FPS)
  SDL.delay (fromIntegral frameDelayMs)

  unless quit $ appLoop renderer font stateVar pty vterm

-- | Run the application
run :: IO ()
run = do
  -- Initialize SDL
  SDL.initializeAll
  TTF.initialize

  -- Create window (sized for terminal)
  let winWidth = fromIntegral $ 20 + termCols * 10 + 20 -- margins + chars
      winHeight = fromIntegral $ 20 + termRows * 18 + 20
  window <-
    SDL.createWindow
      appTitle
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 winWidth winHeight,
          SDL.windowResizable = False
        }

  -- Create renderer
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.defaultRenderer
        { SDL.rendererType = SDL.AcceleratedVSyncRenderer
        }

  -- Load font
  font <- TTF.load fontPath fontSize

  -- Enable text input (with a rect for IME positioning)
  let inputRect = Raw.Rect 20 20 (winWidth - 40) (winHeight - 40)
  SDL.startTextInput inputRect

  -- Initialize VTerm
  vterm <- newVTerm termRows termCols

  -- Spawn shell in PTY
  pty <- spawnShell termRows termCols

  -- Create terminal state
  let termState = newTerminalState vterm pty termRows termCols

  -- Initialize app state
  stateVar <- newAppState termState

  -- Start PTY reader thread
  runningVar <- newTVarIO True
  _ <- forkIO $ ptyReaderThread pty vterm runningVar

  -- Run main loop
  TIO.putStrLn "TEMU starting..."
  appLoop renderer font stateVar pty vterm
    `finally` do
      -- Signal reader thread to stop
      atomically $ writeTVar runningVar False
      threadDelay 10000 -- Give thread time to exit

      -- Cleanup
      TIO.putStrLn "TEMU shutting down..."
      closePTY pty
      freeVTerm vterm
      SDL.stopTextInput
      TTF.free font
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      TTF.quit
      SDL.quit
      TIO.putStrLn "TEMU shutdown complete."
