{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Foreign.Storable (Storable(..))
import Data.IORef (IORef, readIORef, modifyIORef, newIORef, writeIORef)
import Graphics.Wayland.WlRoots.Input (InputDevice, DeviceType(..), inputDeviceType, ButtonState(..))
import Graphics.Wayland.WlRoots.XCursor (WlrXCursor, loadCursorTheme, getCursor, getImages, WlrXCursorImage(..))
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, createCursor, setXCursor, warpCursor, getCursorX, getCursorY, attachInputDevice, cursorGetEvents, CursorEvents(..), attachOutputLayout)
import Graphics.Wayland.WlRoots.Input.Pointer (WlrEventPointerButton(..))
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout, createOutputLayout, addOutputAuto)
import Graphics.Wayland.WlRoots.Output
    ( Output
    , makeOutputCurrent
    , swapOutputBuffers
    , effectiveResolution
    , getTransMatrix
    , setCursor
    )
import Graphics.Wayland.WlRoots.Backend (Backend)

import Graphics.Wayland.Signal (addListener, WlListener(..), ListenerToken)
import Control.Exception (bracket_)

import System.IO (hPutStrLn, stderr)

import Shared

import Control.Monad (forM_)
import Text.XkbCommon.InternalTypes (Keysym, Direction(..))
import Text.XkbCommon.KeysymPatterns

data Color = Color
    { colorR :: Float
    , colorG :: Float
    , colorB :: Float
    , colorA :: Float
    }

changeColor :: Color -> Int -> Float -> Color
changeColor c 0 x = c { colorR = x }
changeColor c 1 x = c { colorG = x }
changeColor c 2 x = c { colorB = x }
changeColor c 3 x = c { colorA = x }
changeColor c i x = changeColor c (i `mod` 3) x

data PointerState = PointerState
    { stateXCursor :: Ptr WlrXCursor
    , stateCursor :: Ptr WlrCursor

    , currentX :: Int
    , currentY :: Int

    , defaultColor :: Color
    , clearColor :: Color

    , stateLayout :: Ptr WlrOutputLayout
    , stateDevices :: [InputDevice]

    , stateToken :: ListenerToken
    }

getInitialState :: IORef PointerState -> IO PointerState
getInitialState ref = do
    let c = Color 0.25 0.25 0.25 1
    cursor <- createCursor
    theme <- loadCursorTheme "default" 16
    xcursor <- getCursor theme "left_ptr"
    layout <- createOutputLayout

    attachOutputLayout cursor layout
    setXCursor cursor xcursor

    let signal = cursorButton $ cursorGetEvents cursor
    tok <- addListener (WlListener $ handleCursorButton ref) signal

    pure $ PointerState
        { stateXCursor = xcursor
        , stateCursor = cursor
        , currentX = 0
        , currentY = 0
        , defaultColor = c
        , clearColor = c

        , stateLayout = layout
        , stateDevices = []

        , stateToken = tok
        }

setCursorImage :: Ptr Output -> Ptr WlrXCursor -> IO ()
setCursorImage output xcursor = do
    images <- getImages xcursor
    image <- peek $ head images

    setCursor
        output
        (xCursorImageBuffer image)
        (xCursorImageWidth image)
        (xCursorImageWidth image)
        (xCursorImageHeight image)
        (xCursorImageHotspotX image)
        (xCursorImageHotspotY image)

foreign import ccall "glClearColor" glClearColor :: Float -> Float -> Float -> Float -> IO ()
foreign import ccall "glClear" glClear :: Int -> IO ()

frameHandler :: IORef PointerState -> Double -> Ptr Output -> IO ()
frameHandler ref _ output = do
    state <- readIORef ref
    let col = clearColor state
    makeOutputCurrent output
    glClearColor (colorR col) (colorG col) (colorB col) (colorA col)
    glClear 0x00004000
    swapOutputBuffers output

handleCursorButton :: IORef PointerState -> Ptr WlrEventPointerButton -> IO ()
handleCursorButton ref event_ptr = do
    state <- readIORef ref
    event <- peek event_ptr

    let value = case eventPointerButtonState event of
                    ButtonPressed -> 1.0
                    ButtonReleased -> 0.25
    let btn = fromIntegral $ eventPointerButtonButton event
    let col = changeColor (clearColor state) btn value

    writeIORef ref state {clearColor = col}

handleInputAdd :: IORef PointerState -> Ptr InputDevice -> IO ()
handleInputAdd ref dev = do
    hPutStrLn stderr "Adding input"
    devType <- inputDeviceType dev
    case devType of
        DevicePointer ptr -> do
            state <- readIORef ref
            attachInputDevice (stateCursor state) dev
        _ -> hPutStrLn stderr "Ignoring input device, since it's not pointer related"

handleOutputAdd :: IORef PointerState -> Ptr Output -> IO FrameHandler
handleOutputAdd ref output = do
    hPutStrLn stderr "Adding output"
    state <- readIORef ref

    addOutputAuto (stateLayout state) output
    setCursorImage output (stateXCursor state)

    x <- getCursorX (stateCursor state)
    y <- getCursorY (stateCursor state)

    _ <- warpCursor (stateCursor state) Nothing x y

    pure $ frameHandler ref


main :: IO ()
main = do
    stateRef <- newIORef undefined
    writeIORef stateRef =<< getInitialState stateRef

    launchCompositor ignoreHooks
        { inputAddHook = handleInputAdd stateRef
        , outputAddHook = handleOutputAdd stateRef
        }
