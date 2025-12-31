{-# LANGUAGE OverloadedStrings #-}

-- | SDL2 rendering functions
module Temu.Render
  ( -- * Text rendering
    renderText,

    -- * Main render function
    render,
  )
where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import Temu.Config
  ( bgColor,
    lineHeight,
    marginLeft,
    marginTop,
    promptColor,
    textColor,
  )
import Temu.State (AppState (..))

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

-- | Main render function - renders the entire application state
render :: SDL.Renderer -> TTF.Font -> AppState -> IO ()
render renderer font state = do
  -- Clear screen with background color
  SDL.rendererDrawColor renderer $= bgColor
  SDL.clear renderer

  -- Render output lines
  mapM_
    ( \(i, line) ->
        renderText
          renderer
          font
          textColor
          (SDL.V2 marginLeft (marginTop + i * lineHeight))
          line
    )
    (zip [0 ..] (outputLines state))

  -- Render input prompt and buffer
  let promptY = marginTop + fromIntegral (length (outputLines state)) * lineHeight + 10
      prompt = "> "
      cursorChar = if cursorVisible state then "█" else " "
      inputLine = prompt <> inputBuffer state <> cursorChar

  renderText renderer font promptColor (SDL.V2 marginLeft promptY) inputLine

  SDL.present renderer
