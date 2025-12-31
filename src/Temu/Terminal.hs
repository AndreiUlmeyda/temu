{-# LANGUAGE OverloadedStrings #-}

-- | Terminal/shell execution functionality
-- This module will later be replaced with libvterm integration
module Temu.Terminal
  ( -- * Command execution
    executeCommand,
    executeCommandWithShell,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO.Handle (hGetContents)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )
import Temu.Config (shellPath)

-- | Execute a command using the default shell and return output
executeCommand :: Text -> IO Text
executeCommand = executeCommandWithShell shellPath

-- | Execute a command with a specific shell and return output
-- Returns the last 2 non-empty lines of combined stdout/stderr
executeCommandWithShell :: FilePath -> Text -> IO Text
executeCommandWithShell shell cmd = do
  let shellProc =
        (proc shell ["-c", T.unpack cmd])
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
