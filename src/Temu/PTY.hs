{-# LANGUAGE OverloadedStrings #-}

-- | PTY (pseudo-terminal) management for spawning and communicating with shell
module Temu.PTY
  ( -- * PTY handle
    PTY (..),

    -- * Lifecycle
    spawnShell,
    closePTY,

    -- * I/O
    readPTY,
    writePTY,

    -- * Utilities
    getUserShell,
    resizePTY,
  )
where

import Control.Exception (SomeException, catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Environment (lookupEnv)
import System.Posix.Pty
  ( Pty,
    closePty,
    readPty,
    resizePty,
    spawnWithPty,
    writePty,
  )
import System.Process (ProcessHandle, terminateProcess)

-- | PTY handle containing the pty and process handle
data PTY = PTY
  { ptyHandle :: !Pty,
    ptyProcess :: !ProcessHandle
  }

-- | Get the user's default shell from $SHELL, fallback to /bin/sh
getUserShell :: IO FilePath
getUserShell = do
  mshell <- lookupEnv "SHELL"
  return $ maybe "/bin/sh" id mshell

-- | Spawn a shell in a PTY with given dimensions (rows, cols)
spawnShell :: Int -> Int -> IO PTY
spawnShell rows cols = do
  shell <- getUserShell
  -- spawnWithPty: search PATH, args, env inherit, dimensions
  (pty, ph) <-
    spawnWithPty
      Nothing -- search PATH
      True -- inherit environment
      shell -- program
      [] -- args (shell will read from rc files)
      (cols, rows) -- (width, height) in characters
  return $ PTY pty ph

-- | Close the PTY and terminate the shell process
closePTY :: PTY -> IO ()
closePTY (PTY pty ph) = do
  closePty pty
  terminateProcess ph `catch` ignoreException
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = return ()

-- | Read available data from the PTY (non-blocking if no data)
readPTY :: PTY -> IO ByteString
readPTY (PTY pty _) = do
  result <- readPty pty `catch` handleReadError
  return result
  where
    handleReadError :: SomeException -> IO ByteString
    handleReadError _ = return BS.empty

-- | Write data to the PTY (keyboard input)
writePTY :: PTY -> ByteString -> IO ()
writePTY (PTY pty _) bs = do
  writePty pty bs `catch` ignoreException
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = return ()

-- | Resize the PTY to new dimensions
resizePTY :: PTY -> Int -> Int -> IO ()
resizePTY (PTY pty _) rows cols = do
  resizePty pty (cols, rows) `catch` ignoreException
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = return ()
