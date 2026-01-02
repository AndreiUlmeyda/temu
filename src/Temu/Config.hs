{-# LANGUAGE OverloadedStrings #-}

-- | Configuration constants for TEMU
module Temu.Config
  ( -- * Window dimensions
    windowWidth,
    windowHeight,

    -- * Colors (classic green phosphor CRT aesthetic)
    bgColor,
    textColor,
    promptColor,

    -- * Font settings
    fontPath,
    fontSize,

    -- * Timing
    cursorBlinkMs,
    frameDelayMs,

    -- * UI layout
    marginLeft,
    marginTop,

    -- * Application info
    appTitle,
  )
where

import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Font as TTF

-- | Window width in pixels (calculated for 80 cols + margins)
windowWidth :: CInt
windowWidth = 840

-- | Window height in pixels (calculated for 24 rows + margins)
windowHeight :: CInt
windowHeight = 472

-- | Background color - near-black with slight green tint #0A0F0A
bgColor :: SDL.V4 Word8
bgColor = SDL.V4 10 15 10 255

-- | Main text color - phosphor green #32FF64
textColor :: SDL.V4 Word8
textColor = SDL.V4 50 255 100 255

-- | Prompt color - slightly dimmer green #50C878
promptColor :: SDL.V4 Word8
promptColor = SDL.V4 80 200 120 255

-- | Path to the font file
fontPath :: FilePath
fontPath = "/System/Library/Fonts/Monaco.ttf"

-- | Font size in points
fontSize :: TTF.PointSize
fontSize = 18

-- | Cursor blink interval in milliseconds
cursorBlinkMs :: Word
cursorBlinkMs = 500

-- | Frame delay in milliseconds (~60 FPS)
frameDelayMs :: Word
frameDelayMs = 16

-- | Left margin in pixels
marginLeft :: CInt
marginLeft = 20

-- | Top margin in pixels
marginTop :: CInt
marginTop = 20

-- | Window title
appTitle :: Text
appTitle = "TEMU - The Terminal Emulator We Have At Home"
