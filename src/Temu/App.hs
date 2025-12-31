{-# LANGUAGE OverloadedStrings #-}

-- | Main application module - wires everything together
module Temu.App
  ( -- * Application entry point
    run,
  )
where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Raw.Types as Raw
import Temu.Config
  ( appTitle,
    cursorBlinkMs,
    fontPath,
    fontSize,
    frameDelayMs,
    windowHeight,
    windowWidth,
  )
import Temu.Input (InputAction (..), applyInputAction, classifyEvent)
import Temu.Render (render)
import Temu.State
  ( AppState (..),
    AppStateVar,
    modifyAppState_,
    newAppState,
    readAppState,
    setOutputLines,
    updateCursorBlink,
  )
import Temu.Terminal (executeCommand)

-- | Process a single event and return whether to quit
processEvent :: SDL.Event -> AppStateVar -> IO Bool
processEvent event stateVar = do
  state <- readAppState stateVar
  let action = classifyEvent (SDL.eventPayload event) state

  case action of
    Quit -> pure True
    SubmitCommand cmd -> do
      -- Execute command and update output
      result <- executeCommand cmd
      let newOutput =
            take 4 $
              filter (not . T.null) (T.lines result)
                ++ ["> " <> cmd]
                ++ outputLines state
      modifyAppState_ stateVar (setOutputLines (take 4 newOutput) . applyInputAction action)
      pure False
    _ -> do
      modifyAppState_ stateVar (applyInputAction action)
      pure False

-- | Main application loop
appLoop :: SDL.Renderer -> TTF.Font -> AppStateVar -> IO ()
appLoop renderer font stateVar = do
  -- Poll and process events
  events <- SDL.pollEvents
  quitSignals <- mapM (`processEvent` stateVar) events
  let quit = or quitSignals

  -- Update cursor blink
  currentTime <- SDL.ticks
  modifyAppState_ stateVar (updateCursorBlink currentTime (fromIntegral cursorBlinkMs))

  -- Render current state
  state <- readAppState stateVar
  render renderer font state

  -- Frame delay (~60 FPS)
  SDL.delay (fromIntegral frameDelayMs)

  unless quit $ appLoop renderer font stateVar

-- | Run the application
run :: IO ()
run = do
  -- Initialize SDL
  SDL.initializeAll
  TTF.initialize

  -- Create window
  window <-
    SDL.createWindow
      appTitle
      SDL.defaultWindow
        { SDL.windowInitialSize = SDL.V2 windowWidth windowHeight,
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
  let inputRect = Raw.Rect 20 150 760 30
  SDL.startTextInput inputRect

  -- Initialize state
  stateVar <- newAppState

  -- Run main loop
  TIO.putStrLn "TEMU starting..."
  appLoop renderer font stateVar

  -- Cleanup
  SDL.stopTextInput
  TTF.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  TTF.quit
  SDL.quit
  TIO.putStrLn "TEMU shutdown complete."
