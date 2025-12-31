-- | TEMU - The Terminal Emulator We Have At Home
-- A retro sci-fi terminal emulator
module Main (main) where

import qualified Temu.App

-- | Application entry point
main :: IO ()
main = Temu.App.run
