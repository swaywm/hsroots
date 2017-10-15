module Input.Keyboard
where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.Server
    ( DisplayServer
    , displayTerminate
    )
import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    )
import Graphics.Wayland.WlRoots.Seat (WlrSeat, attachKeyboard)
import Graphics.Wayland.WlRoots.Backend.Multi (getSession')
import Graphics.Wayland.WlRoots.Backend.Session (changeVT)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard)
import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr
    , EventKey (..)
    , KeyState (..)
    , setKeymap
    , getKeystate
    )
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )
import Data.List (intercalate)
import System.IO (hPutStr, hPutStrLn, stderr)
import Control.Monad (forM_)

import Text.XkbCommon.Keymap
import Text.XkbCommon.Keysym
import Text.XkbCommon.Types
import Text.XkbCommon.Context
import Text.XkbCommon.KeyboardState
import Text.XkbCommon.KeycodeList
import Text.XkbCommon.KeysymPatterns

data Keyboard = Keyboard
    { keyboardDevice :: Ptr WlrKeyboard

    }

keyStateToDirection :: KeyState -> Direction
keyStateToDirection KeyReleased = keyUp
keyStateToDirection KeyPressed  = keyDown


switchVT :: Ptr Backend -> Word -> IO ()
switchVT backend vt = do
    mSession <- getSession' backend
    case mSession of
        Nothing -> pure ()
        Just s -> changeVT s vt

handleKeyPress :: DisplayServer -> Ptr Backend -> Ptr WlrKeyboard -> Ptr EventKey -> IO ()
handleKeyPress dsp backend keyboard ptr = do
    event <- peek ptr
    let keycode = fromEvdev . fromIntegral . keyCode $ event
    keyState <- getKeystate keyboard
    syms <- getStateSymsI keyState keycode
    let keyDir = (keyStateToDirection $ state event)
    hPutStrLn stderr . intercalate "," $ map keysymName syms
    forM_ syms $ \sym -> case sym of
        Keysym_Escape -> displayTerminate dsp
        -- Would be cooler if this wasn't a listing of VTs (probably TH)
        Keysym_XF86Switch_VT_1  -> switchVT backend 1
        Keysym_XF86Switch_VT_2  -> switchVT backend 2
        Keysym_XF86Switch_VT_3  -> switchVT backend 3
        Keysym_XF86Switch_VT_4  -> switchVT backend 4
        Keysym_XF86Switch_VT_5  -> switchVT backend 5
        Keysym_XF86Switch_VT_6  -> switchVT backend 6
        Keysym_XF86Switch_VT_7  -> switchVT backend 7
        Keysym_XF86Switch_VT_8  -> switchVT backend 8
        Keysym_XF86Switch_VT_9  -> switchVT backend 9
        Keysym_XF86Switch_VT_10 -> switchVT backend 10
        Keysym_XF86Switch_VT_11 -> switchVT backend 11
        Keysym_XF86Switch_VT_12 -> switchVT backend 12
        _ -> pure ()

handleKeyboardAdd :: DisplayServer -> Ptr Backend -> Ptr WlrSeat -> Ptr InputDevice -> Ptr WlrKeyboard -> IO ()
handleKeyboardAdd dsp backend seat dev ptr = do
    let signals = getKeySignals ptr

    (Just cxt) <- newContext defaultFlags
    (Just keymap) <- newKeymapFromNamesI cxt noPrefs

    setKeymap ptr keymap

    handler <- addListener (WlListener $ handleKeyPress dsp backend ptr) (keySignalKey signals)
    sptr <- newStablePtr handler
    poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)
    attachKeyboard seat dev



handleKeyboardRemove :: Ptr WlrKeyboard -> IO ()
handleKeyboardRemove ptr = do
    sptr <- peek (getKeyDataPtr ptr)
    freeStablePtr $ castPtrToStablePtr sptr
