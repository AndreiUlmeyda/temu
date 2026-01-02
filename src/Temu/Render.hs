{-# LANGUAGE OverloadedStrings #-}

-- | SDL2 rendering functions for terminal display
module Temu.Render
  ( -- * Text rendering
    renderText,
    renderChar,

    -- * Main render function
    render,

    -- * Grid rendering
    renderCellGrid,
  )
where

import Control.Monad (forM_, when)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import Temu.Config
  ( bgColor,
    marginLeft,
    marginTop,
  )
import Temu.State
  ( AppState (..),
    Cell (..),
    Color (..),
    TerminalState (..),
  )

-- | Convert VTerm Color to SDL color
toSDLColor :: Color -> SDL.V4 Word8
toSDLColor (Color r g b) = SDL.V4 r g b 255

-- | Character width in pixels (monospace assumption)
charWidth :: CInt
charWidth = 10

-- | Character height in pixels
charHeight :: CInt
charHeight = 18

-- | Render text at a position with given color
renderText :: SDL.Renderer -> TTF.Font -> SDL.V4 Word8 -> SDL.V2 CInt -> Text -> IO ()
renderText renderer font color pos txt = do
  when (not $ T.null txt) $ do
    surface <- TTF.blended font color txt
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    SDL.TextureInfo {SDL.textureWidth = w, SDL.textureHeight = h} <- SDL.queryTexture texture
    let rect = SDL.Rectangle (SDL.P pos) (SDL.V2 w h)
    SDL.copy renderer texture Nothing (Just rect)
    SDL.destroyTexture texture

-- | Render a single character at a position
renderChar :: SDL.Renderer -> TTF.Font -> Cell -> CInt -> CInt -> Bool -> IO ()
renderChar renderer font cell x y isCursor = do
  let fg = if isCursor then toSDLColor (cellBg cell) else toSDLColor (cellFg cell)
      bg = if isCursor then toSDLColor (cellFg cell) else toSDLColor (cellBg cell)
      ch = cellChar cell

  -- Draw background rectangle
  SDL.rendererDrawColor renderer $= bg
  let bgRect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 charWidth charHeight)
  SDL.fillRect renderer (Just bgRect)

  -- Draw character if not space
  when (ch /= ' ' && ch /= '\0') $ do
    let txt = T.singleton ch
    surface <- TTF.blended font fg txt
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    SDL.TextureInfo {SDL.textureWidth = w, SDL.textureHeight = h} <- SDL.queryTexture texture
    let charRect = SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h)
    SDL.copy renderer texture Nothing (Just charRect)
    SDL.destroyTexture texture

-- | Render the entire cell grid
renderCellGrid ::
  SDL.Renderer ->
  TTF.Font ->
  Vector (Vector Cell) ->
  (Int, Int) -> -- cursor position (row, col)
  Bool -> -- cursor visible
  IO ()
renderCellGrid renderer font grid (cursorRow, cursorCol) cursorVis = do
  forM_ (zip [0 ..] (V.toList grid)) $ \(row, rowCells) -> do
    forM_ (zip [0 ..] (V.toList rowCells)) $ \(col, cell) -> do
      let x = marginLeft + fromIntegral col * charWidth
          y = marginTop + fromIntegral row * charHeight
          isCursor = cursorVis && row == cursorRow && col == cursorCol
      renderChar renderer font cell x y isCursor

-- | Main render function - renders the entire application state
render :: SDL.Renderer -> TTF.Font -> AppState -> IO ()
render renderer font state = do
  -- Clear screen with background color
  SDL.rendererDrawColor renderer $= bgColor
  SDL.clear renderer

  -- Render the terminal cell grid
  let termState = terminal state
      grid = termCellGrid termState
      cursorPos = termCursorPos termState

  renderCellGrid renderer font grid cursorPos (cursorVisible state)

  SDL.present renderer
