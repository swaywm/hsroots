{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Foreign.StablePtr (newStablePtr, castStablePtrToPtr)
import Foreign.Storable (Storable(peek, poke))
import Data.IORef (IORef, readIORef, modifyIORef, newIORef, writeIORef)
import Cat (getCatTexture)
import Graphics.Wayland.WlRoots.Render
    ( Texture
    , Renderer
    , doRender
    , getMatrix
    , renderWithMatrix
    )
import System.Clock
    ( toNanoSecs
    , getTime
    , Clock(Monotonic)
    )
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix)
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)
import Data.Maybe (listToMaybe)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Backend
    ( Backend, backendAutocreate, backendStart
    , BackendSignals(..), backendGetSignals
    )
import Graphics.Wayland.WlRoots.Output
    ( Output
    , getName
    , getModes
    , setOutputMode
    , effectiveResolution
    , makeOutputCurrent
    , swapOutputBuffers
    , getTransMatrix

    , OutputSignals(..)
    , getOutputSignals
    , getDataPtr

    , transformOutput
    )
import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    )
import Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr
    , EventKey (..)
    , KeyState (..)
    )
import Graphics.Wayland.Server
    ( DisplayServer
    , displayCreate
    , displayRun
    , displayTerminate
    , outputTransform180
    )
import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    , removeListener
    )

import Text.XkbCommon.Keymap
import Text.XkbCommon.Keysym
import Text.XkbCommon.Types
import Text.XkbCommon.Context
import Text.XkbCommon.KeyboardState
import Text.XkbCommon.KeycodeList
import Text.XkbCommon.KeysymList

import Control.Monad (forM_, when)
import Control.Exception (bracket_)
import Data.List (intercalate)

import System.IO (hPutStr, hPutStrLn, stderr)


keyStateToDirection :: KeyState -> Direction
keyStateToDirection KeyReleased = keyUp
keyStateToDirection KeyPressed  = keyDown

data Handlers = Handlers ListenerToken ListenerToken ListenerToken ListenerToken
data OutputState = OutputState
    { xOffset :: Int
    , yOffset :: Int
    , lastFrame :: Integer
    }

data CatRenderer = CatRenderer (Ptr Renderer) (Ptr Texture)

renderOn :: Ptr Output -> Ptr Renderer -> IO a -> IO a
renderOn output rend act = bracket_ 
    (makeOutputCurrent output)
    (swapOutputBuffers output)
    (doRender rend output act)

frameHandler :: IORef OutputState -> IORef CatRenderer -> Ptr Output -> IO ()
frameHandler ref cref output = do
    state <- readIORef ref
    (CatRenderer rend tex) <- readIORef cref
    (width, height) <- effectiveResolution output
    -- The offsets on the x axis
    let xs = [-128 + xOffset state, xOffset state .. width]
    -- The offsets on the y axis
    let ys = [-128 + yOffset state, yOffset state .. height]
    -- All offsets in the 2 dimensional plane
    let zs = [ (x, y) | x <- xs, y<- ys]

    renderOn output rend $ withMatrix $ \matrix -> forM_ zs $ \(x, y) -> do
        getMatrix tex matrix (getTransMatrix output) x y
        renderWithMatrix rend tex matrix

    time <- toNanoSecs <$> getTime Monotonic
    let timeDiff = time - lastFrame state
    let secs :: Double = fromIntegral timeDiff / 1e9
    let adjust = floor $ 128 * secs

    modifyIORef ref (\(OutputState x y _) -> OutputState ((x + adjust) `mod` 128) ((y + adjust) `mod` 128) time)


getCatRenderer :: Ptr Backend -> IO (CatRenderer)
getCatRenderer backend = do
    renderer <- rendererCreate backend
    texture <- getCatTexture renderer
    pure $ CatRenderer renderer texture

handleOutputAdd :: IORef CatRenderer -> Ptr Output -> IO ()
handleOutputAdd cat output = do
    putStr "Found output: "
    putStrLn =<< getName output

    modes <- getModes output
    case listToMaybe modes of
        Nothing -> pure ()
        Just x -> setOutputMode x output

    ref <- newIORef (OutputState 0 0 0)

    let signals = getOutputSignals output
    handler <- addListener (WlListener (\_ -> frameHandler ref cat output)) (outSignalFrame signals)
    transformOutput output outputTransform180

    sptr <- newStablePtr handler
    poke (getDataPtr output) (castStablePtrToPtr sptr)

handleKeyPress :: DisplayServer -> KeyboardState -> Ptr EventKey -> IO ()
handleKeyPress dsp keyState ptr = do
    hPutStr stderr "Some key was pressed: "
    event <- peek ptr
    let keycode = fromEvdev . fromIntegral . keyCode $ event
    syms <- getStateSyms keyState keycode
    _ <- updateKeyboardStateKey keyState keycode (keyStateToDirection $ state event)
    hPutStrLn stderr . intercalate "," $ map keysymName syms
    forM_ syms $ \sym -> do
        when (sym == keysym_Escape) (displayTerminate dsp)

handleKeyboardAdd :: DisplayServer -> Ptr WlrKeyboard -> IO ()
handleKeyboardAdd dsp ptr = do
    let signals = getKeySignals ptr

    (Just cxt) <- newContext defaultFlags
    (Just keymap) <- newKeymapFromNames cxt noPrefs
    keyState <- newKeyboardState keymap

    handler <- addListener (WlListener $ handleKeyPress dsp keyState) (keySignalKey signals)
    sptr <- newStablePtr handler
    poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)
    pure ()

handleInputAdd :: DisplayServer -> Ptr InputDevice -> IO ()
handleInputAdd dsp ptr = do
    putStr "Found a new input of type: "
    iType <- inputDeviceType ptr
    print iType
    case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd dsp kptr
        _ -> pure ()

addSignalHandlers :: DisplayServer -> IORef CatRenderer -> Ptr Backend -> IO Handlers
addSignalHandlers dsp cref ptr =
    let signals = backendGetSignals ptr
     in Handlers
        <$> addListener (WlListener $ handleInputAdd dsp) (inputAdd signals)
        <*> addListener (WlListener (\_ -> putStrLn "Lost an input")) (inputRemove signals)
        <*> addListener (WlListener (handleOutputAdd cref)) (outputAdd signals)
        <*> addListener (WlListener (\_ -> putStrLn "Lost an output")) (outputRemove signals)

main :: IO ()
main = do
    catRef <- newIORef undefined
    display <- displayCreate
    backend <- backendAutocreate display

    handlers <- addSignalHandlers display catRef backend

    backendStart backend

    renderer <- getCatRenderer backend
    writeIORef catRef renderer

    displayRun display

    let Handlers h1 h2 h3 h4 = handlers
    removeListener h1
    removeListener h2
    removeListener h3
    removeListener h4
