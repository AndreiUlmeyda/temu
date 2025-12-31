{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless, when)
import Data.IORef
  ( IORef,
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word32, Word8)
import Foreign.C.Types (CInt)
import GHC.IO.Handle (hGetContents)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Raw.Types as Raw
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )

windowWidth, windowHeight :: CInt
windowWidth = 800
windowHeight = 200

-- | Colors - classic green phosphor CRT aesthetic
bgColor :: SDL.V4 Word8
bgColor = SDL.V4 10 15 10 255 -- Near-black with slight green tint

textColor :: SDL.V4 Word8
textColor = SDL.V4 50 255 100 255 -- Phosphor green

promptColor :: SDL.V4 Word8
promptColor = SDL.V4 80 200 120 255 -- Slightly dimmer green for prompt

-- | Application state
data AppState = AppState
  { inputBuffer :: Text, -- Current input line
    outputLines :: [Text], -- Last few lines of output
    cursorVisible :: Bool, -- Blinking cursor state
    lastBlinkTime :: Word32 -- For cursor blink timing
  }

initialState :: AppState
initialState =
  AppState
    { inputBuffer = "",
      outputLines = ["TEMU v0.1 - The Terminal Emulator We Have At Home", "Ready."],
      cursorVisible = True,
      lastBlinkTime = 0
    }

-- | Execute a command in fish shell and return output
executeCommand :: Text -> IO Text
executeCommand cmd = do
  let shellProc =
        (proc "/opt/homebrew/bin/fish" ["-c", T.unpack cmd])
          { std_out = CreatePipe,
            std_err = CreatePipe
          }
  (_, Just hout, Just herr, ph) <- createProcess shellProc
  outStr <- hGetContents hout
  errStr <- hGetContents herr
  _ <- waitForProcess ph
  let output = outStr ++ errStr
      linesOut = lines output
      -- Take last 2 non-empty lines
      relevantLines = take 2 $ reverse $ filter (not . null) linesOut
  pure $ T.pack $ unlines $ reverse relevantLines

-- | Render text at a position
renderText :: SDL.Renderer -> TTF.Font -> SDL.V4 Word8 -> SDL.V2 CInt -> Text -> IO ()
renderText renderer font color pos txt = do
  unless (T.null txt) $ do
    surface <- TTF.solid font color txt
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    SDL.TextureInfo {SDL.textureWidth = w, SDL.textureHeight = h} <- SDL.queryTexture texture
    let rect = SDL.Rectangle (SDL.P pos) (SDL.V2 w h)
    SDL.copy renderer texture Nothing (Just rect)
    SDL.destroyTexture texture

-- | Main render function
render :: SDL.Renderer -> TTF.Font -> AppState -> IO ()
render renderer font state = do
  -- Clear screen with background color
  SDL.rendererDrawColor renderer $= bgColor
  SDL.clear renderer

  -- Render output lines
  let yStart = 20
      lineHeight = 28
  mapM_
    (\(i, line) -> renderText renderer font textColor (SDL.V2 20 (yStart + i * lineHeight)) line)
    (zip [0 ..] (outputLines state))

  -- Render input prompt and buffer
  let promptY = yStart + fromIntegral (length (outputLines state)) * lineHeight + 10
      prompt = "> "
      cursorChar = if cursorVisible state then "█" else " "
      inputLine = prompt <> inputBuffer state <> cursorChar

  renderText renderer font promptColor (SDL.V2 20 promptY) inputLine

  SDL.present renderer

-- | Handle keyboard events
handleEvent :: SDL.Event -> IORef AppState -> IO Bool
handleEvent event stateRef = do
  case SDL.eventPayload event of
    SDL.QuitEvent -> pure True
    SDL.KeyboardEvent keyboardEvent -> do
      when (SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed) $ do
        let keysym = SDL.keyboardEventKeysym keyboardEvent
            keycode = SDL.keysymKeycode keysym
        case keycode of
          SDL.KeycodeReturn -> do
            state <- readIORef stateRef
            let cmd = inputBuffer state
            unless (T.null cmd) $ do
              result <- executeCommand cmd
              let newOutput =
                    take 4 $
                      filter (not . T.null) (T.lines result)
                        ++ ["> " <> cmd]
                        ++ outputLines state
              writeIORef stateRef $
                state
                  { inputBuffer = "",
                    outputLines = take 4 newOutput
                  }
          SDL.KeycodeBackspace -> do
            modifyIORef' stateRef $ \s ->
              s {inputBuffer = if T.null (inputBuffer s) then "" else T.init (inputBuffer s)}
          SDL.KeycodeEscape -> pure () -- Could quit here
          _ -> pure ()
      pure False
    SDL.TextInputEvent textEvent -> do
      let txt = SDL.textInputEventText textEvent
      modifyIORef' stateRef $ \s -> s {inputBuffer = inputBuffer s <> txt}
      pure False
    _ -> pure False

-- | Main application loop
appLoop :: SDL.Renderer -> TTF.Font -> IORef AppState -> IO ()
appLoop renderer font stateRef = do
  events <- SDL.pollEvents
  quit <- or <$> mapM (`handleEvent` stateRef) events

  -- Update cursor blink (every 500ms)
  currentTime <- SDL.ticks
  state <- readIORef stateRef
  when (currentTime - lastBlinkTime state > 500) $ do
    modifyIORef' stateRef $ \s ->
      s
        { cursorVisible = not (cursorVisible s),
          lastBlinkTime = currentTime
        }

  -- Render
  state' <- readIORef stateRef
  render renderer font state'

  -- Small delay to prevent CPU spinning
  SDL.delay 16 -- ~60 FPS
  unless quit $ appLoop renderer font stateRef

main :: IO ()
main = do
  -- Initialize SDL
  SDL.initializeAll
  TTF.initialize

  -- Create window
  window <-
    SDL.createWindow
      "TEMU - The Terminal Emulator We Have At Home"
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

  -- Load a monospace font (using system font for now)
  -- You'll want to bundle a proper retro font later
  font <- TTF.load "/System/Library/Fonts/Monaco.ttf" 18

  -- Enable text input (with a rect for IME positioning)
  let inputRect = Raw.Rect 20 150 760 30
  SDL.startTextInput inputRect

  -- Initialize state
  stateRef <- newIORef initialState

  -- Run main loop
  TIO.putStrLn "TEMU starting..."
  appLoop renderer font stateRef

  -- Cleanup
  SDL.stopTextInput
  TTF.free font
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  TTF.quit
  SDL.quit
  TIO.putStrLn "TEMU shutdown complete."
