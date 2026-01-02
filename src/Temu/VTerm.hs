{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | FFI bindings to libvterm for terminal emulation
module Temu.VTerm
  ( -- * Opaque types
    VTerm,
    VTermScreen,

    -- * Lifecycle
    newVTerm,
    freeVTerm,
    getScreen,

    -- * Input
    inputWrite,

    -- * Screen queries
    Cell (..),
    Color (..),
    getCell,
    getCursorPos,
    getSize,

    -- * Screen operations
    flushOutput,
    setOutputCallback,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign
import Foreign.C.Types

-- | Opaque VTerm handle
newtype VTerm = VTerm (Ptr VTerm)
  deriving (Eq, Show, Storable)

-- | Opaque VTermScreen handle
newtype VTermScreen = VTermScreen (Ptr VTermScreen)
  deriving (Eq, Show, Storable)

-- | Position in the terminal grid
data VTermPos = VTermPos
  { posRow :: !CInt,
    posCol :: !CInt
  }
  deriving (Show, Eq)

instance Storable VTermPos where
  sizeOf _ = 8 -- 2 * sizeof(int)
  alignment _ = 4
  peek ptr = do
    r <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    return $ VTermPos r c
  poke ptr (VTermPos r c) = do
    pokeByteOff ptr 0 r
    pokeByteOff ptr 4 c

-- | RGB color
data Color = Color
  { colorR :: !Word8,
    colorG :: !Word8,
    colorB :: !Word8
  }
  deriving (Show, Eq)

-- | Default foreground color - phosphor green #32FF64
defaultFg :: Color
defaultFg = Color 50 255 100

-- | Default background color - near-black with green tint #0A0F0A
defaultBg :: Color
defaultBg = Color 10 15 10

-- | A terminal cell with character and attributes
data Cell = Cell
  { cellChar :: !Char,
    cellFg :: !Color,
    cellBg :: !Color,
    cellBold :: !Bool,
    cellItalic :: !Bool,
    cellUnderline :: !Bool,
    cellReverse :: !Bool
  }
  deriving (Show, Eq)

-- | Default empty cell
emptyCell :: Cell
emptyCell = Cell ' ' defaultFg defaultBg False False False False

-- VTermScreenCell is a complex struct, we'll extract what we need
-- The struct layout varies by libvterm version, so we use helper functions

-- | Foreign imports
foreign import ccall unsafe "vterm_new"
  c_vterm_new :: CInt -> CInt -> IO (Ptr VTerm)

foreign import ccall unsafe "vterm_free"
  c_vterm_free :: Ptr VTerm -> IO ()

foreign import ccall unsafe "vterm_obtain_screen"
  c_vterm_obtain_screen :: Ptr VTerm -> IO (Ptr VTermScreen)

foreign import ccall unsafe "vterm_screen_reset"
  c_vterm_screen_reset :: Ptr VTermScreen -> CInt -> IO ()

foreign import ccall unsafe "vterm_input_write"
  c_vterm_input_write :: Ptr VTerm -> Ptr CChar -> CSize -> IO CSize

-- Using wrapper because vterm_screen_get_cell takes VTermPos by value
foreign import ccall unsafe "vterm_screen_get_cell_wrapper"
  c_vterm_screen_get_cell :: Ptr VTermScreen -> CInt -> CInt -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "vterm_state_get_cursorpos"
  c_vterm_state_get_cursorpos :: Ptr () -> Ptr VTermPos -> IO ()

foreign import ccall unsafe "vterm_obtain_state"
  c_vterm_obtain_state :: Ptr VTerm -> IO (Ptr ())

foreign import ccall unsafe "vterm_get_size"
  c_vterm_get_size :: Ptr VTerm -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe "vterm_output_read"
  c_vterm_output_read :: Ptr VTerm -> Ptr CChar -> CSize -> IO CSize

-- Type for output callback (for future use)
type OutputCallback = Ptr CChar -> CSize -> Ptr () -> IO CSize

foreign import ccall "wrapper"
  _mkOutputCallback :: OutputCallback -> IO (FunPtr OutputCallback)

foreign import ccall unsafe "vterm_set_utf8"
  c_vterm_set_utf8 :: Ptr VTerm -> CInt -> IO ()

foreign import ccall unsafe "vterm_screen_enable_altscreen"
  c_vterm_screen_enable_altscreen :: Ptr VTermScreen -> CInt -> IO ()

foreign import ccall unsafe "vterm_screen_flush_damage"
  c_vterm_screen_flush_damage :: Ptr VTermScreen -> IO ()

-- | Create a new VTerm instance with given rows and columns
newVTerm :: Int -> Int -> IO VTerm
newVTerm rows cols = do
  ptr <- c_vterm_new (fromIntegral rows) (fromIntegral cols)
  if ptr == nullPtr
    then error "Failed to create VTerm"
    else do
      -- Enable UTF-8 mode
      c_vterm_set_utf8 ptr 1
      -- Get and reset screen
      screenPtr <- c_vterm_obtain_screen ptr
      -- Enable alternate screen buffer (for apps that use it like fish, vim, etc.)
      c_vterm_screen_enable_altscreen screenPtr 1
      c_vterm_screen_reset screenPtr 1
      return $ VTerm ptr

-- | Free a VTerm instance
freeVTerm :: VTerm -> IO ()
freeVTerm (VTerm ptr) = c_vterm_free ptr

-- | Get the screen from a VTerm
getScreen :: VTerm -> IO VTermScreen
getScreen (VTerm ptr) = VTermScreen <$> c_vterm_obtain_screen ptr

-- | Write input bytes to VTerm (from PTY output)
inputWrite :: VTerm -> ByteString -> IO Int
inputWrite vt@(VTerm ptr) bs = do
  BSU.unsafeUseAsCStringLen bs $ \(cstr, len) -> do
    written <- c_vterm_input_write ptr cstr (fromIntegral len)
    -- Flush damage to ensure screen state is updated
    screen <- getScreen vt
    let (VTermScreen screenPtr) = screen
    c_vterm_screen_flush_damage screenPtr
    return $ fromIntegral written

-- | Get a cell at the given position
-- Returns emptyCell if position is out of bounds
getCell :: VTermScreen -> Int -> Int -> IO Cell
getCell (VTermScreen ptr) row col = do
  -- Allocate buffer for cell data (VTermScreenCell is ~64 bytes)
  allocaBytes 128 $ \cellPtr -> do
    result <- c_vterm_screen_get_cell ptr (fromIntegral row) (fromIntegral col) cellPtr
    if result == 0
      then return emptyCell
      else do
        -- Extract character (first 4 bytes are chars array, max 6 uint32_t)
        charCode <- peekByteOff cellPtr 0 :: IO Word32
        let ch = if charCode == 0 then ' ' else toEnum (fromIntegral charCode)

        -- VTermScreenCell layout (libvterm 0.3.x):
        -- chars[6]: offset 0, size 24
        -- width:    offset 24, size 1
        -- padding:  offset 25-27, size 3 (alignment)
        -- attrs:    offset 28, size 4
        -- fg:       offset 32, size 4 (VTermColor: type, r, g, b)
        -- bg:       offset 36, size 4

        -- Extract attributes (at offset 28)
        attrs <- peekByteOff cellPtr 28 :: IO Word32
        let bold = testBit attrs 0
            italic = testBit attrs 3
            underline = testBit attrs 1 || testBit attrs 2
            reverse_ = testBit attrs 5

        -- Extract foreground color (at offset 32)
        -- VTermColor: type at +0, red at +1, green at +2, blue at +3
        fgType <- peekByteOff cellPtr 32 :: IO Word8
        fgR <- peekByteOff cellPtr 33 :: IO Word8
        fgG <- peekByteOff cellPtr 34 :: IO Word8
        fgB <- peekByteOff cellPtr 35 :: IO Word8
        let fg = if fgType == 0 then defaultFg else Color fgR fgG fgB

        -- Extract background color (at offset 36)
        bgType <- peekByteOff cellPtr 36 :: IO Word8
        bgR <- peekByteOff cellPtr 37 :: IO Word8
        bgG <- peekByteOff cellPtr 38 :: IO Word8
        bgB <- peekByteOff cellPtr 39 :: IO Word8
        let bg = if bgType == 0 then defaultBg else Color bgR bgG bgB

        return $ Cell ch fg bg bold italic underline reverse_

-- | Get the cursor position (row, col)
getCursorPos :: VTerm -> IO (Int, Int)
getCursorPos (VTerm ptr) = do
  statePtr <- c_vterm_obtain_state ptr
  alloca $ \posPtr -> do
    c_vterm_state_get_cursorpos statePtr posPtr
    VTermPos row col <- peek posPtr
    return (fromIntegral row, fromIntegral col)

-- | Get the terminal size (rows, cols)
getSize :: VTerm -> IO (Int, Int)
getSize (VTerm ptr) = do
  alloca $ \rowsPtr -> do
    alloca $ \colsPtr -> do
      c_vterm_get_size ptr rowsPtr colsPtr
      rows <- peek rowsPtr
      cols <- peek colsPtr
      return (fromIntegral rows, fromIntegral cols)

-- | Read output from VTerm (to send to PTY)
flushOutput :: VTerm -> IO ByteString
flushOutput (VTerm ptr) = do
  allocaBytes 4096 $ \buf -> do
    len <- c_vterm_output_read ptr buf 4096
    if len > 0
      then BS.packCStringLen (buf, fromIntegral len)
      else return BS.empty

-- | Set output callback (called when VTerm wants to send data to PTY)
setOutputCallback :: VTerm -> (ByteString -> IO ()) -> IO ()
setOutputCallback _ _ = return () -- TODO: implement if needed
