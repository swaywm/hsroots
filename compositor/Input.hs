module Input
    ( Input (..)
    , inputCreate
    )
where

import Input.Keyboard
import Input.Pointer
import Input.Cursor
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Input
    ( InputDevice
    , inputDeviceType
    , DeviceType(..)
    )
import Graphics.Wayland.WlRoots.Seat (WlrSeat, createSeat, setSeatCapabilities)
import Graphics.Wayland.WlRoots.XCursor (WlrXCursorTheme, WlrXCursor, loadCursorTheme, getCursor)
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, setXCursor)
import Graphics.Wayland.Server (DisplayServer(..), seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.WlRoots.Backend (Backend, backendGetSignals, BackendSignals(..))
import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    )

data Input = Input
    { inputCursorTheme :: Ptr WlrXCursorTheme
    , inputXCursor :: Ptr WlrXCursor
    , inputCursor :: Cursor
    , inputSeat :: Ptr WlrSeat
    , inputAddToken :: ListenerToken
    }

handleInputAdd :: Ptr WlrCursor -> DisplayServer -> Ptr Backend -> Ptr InputDevice -> IO ()
handleInputAdd cursor dsp backend ptr = do
    putStr "Found a new input of type: "
    iType <- inputDeviceType ptr
    print iType
    case iType of
        (DeviceKeyboard kptr) -> handleKeyboardAdd dsp backend kptr
        (DevicePointer pptr) -> handlePointer cursor ptr pptr
        _ -> pure ()

inputCreate :: DisplayServer -> Ptr WlrOutputLayout -> Ptr Backend -> IO Input
inputCreate display layout backend = do
    theme <- loadCursorTheme "default" 16
    xcursor <- getCursor theme "left_ptr"
    seat <- createSeat display "seat0"
    cursor <- cursorCreate layout

    setSeatCapabilities seat [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]
    setXCursor (cursorRoots cursor) xcursor

    let signals = backendGetSignals backend
    tok <- addListener (WlListener $ handleInputAdd (cursorRoots cursor) display backend) (inputAdd signals)

    pure Input
        { inputCursorTheme = theme
        , inputXCursor = xcursor
        , inputCursor = cursor
        , inputSeat = seat
        , inputAddToken = tok
        }
