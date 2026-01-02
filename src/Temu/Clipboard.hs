{-# LANGUAGE OverloadedStrings #-}

-- | Clipboard operations for copy/paste functionality
module Temu.Clipboard
  ( -- * Text extraction
    extractSelectedText,

    -- * Clipboard operations
    copyToClipboard,
    pasteFromClipboard,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified SDL
import Temu.State (Cell (..), Selection (..), normalizeSelection)

-- | Extract text from the cell grid within the selection bounds
-- Handles multi-line selections properly, trimming trailing spaces per line
extractSelectedText :: Vector (Vector Cell) -> Selection -> Text
extractSelectedText grid sel =
  let ((r1, c1), (r2, c2)) = normalizeSelection sel
      numRows = V.length grid
      -- Extract lines within selection
      extractLine row
        | row < 0 || row >= numRows = ""
        | otherwise =
            let rowCells = grid V.! row
                numCols = V.length rowCells
                -- Determine column range for this row
                (startCol, endCol)
                  | row == r1 && row == r2 = (c1, min c2 (numCols - 1)) -- Single line
                  | row == r1 = (c1, numCols - 1) -- First line of multi-line
                  | row == r2 = (0, min c2 (numCols - 1)) -- Last line of multi-line
                  | otherwise = (0, numCols - 1) -- Middle line
                -- Extract characters in range
                chars = [cellChar (rowCells V.! col) | col <- [startCol .. endCol], col < numCols]
             in T.stripEnd $ T.pack chars
      -- Build all lines
      lines' = map extractLine [r1 .. r2]
   in T.intercalate "\n" lines'

-- | Copy text to the system clipboard
copyToClipboard :: Text -> IO ()
copyToClipboard txt = SDL.setClipboardText txt

-- | Get text from the system clipboard (if available)
pasteFromClipboard :: IO (Maybe Text)
pasteFromClipboard = do
  hasText <- SDL.hasClipboardText
  if hasText
    then Just <$> SDL.getClipboardText
    else return Nothing

