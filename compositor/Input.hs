module Input
    ( Input (..)
    , inputCreate
    )
where

import Input.Keyboard
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Seat (WlrSeat, createSeat, setSeatCapabilities)
import Graphics.Wayland.WlRoots.XCursor (WlrXCursorTheme, WlrXCursor, loadCursorTheme, getCursor, getImages)
import Graphics.Wayland.WlRoots.Cursor (WlrCursor, createCursor, setXCursor, attachOutputLayout)
import Graphics.Wayland.Server (DisplayServer(..), seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.WlRoots.Backend (Backend, backendGetSignals, BackendSignals(..))

data Input = Input
    { inputCursorTheme :: Ptr WlrXCursorTheme
    , inputXCursor :: Ptr WlrXCursor
    , inputCursor :: Ptr WlrCursor
    , inputSeat :: Ptr WlrSeat
    }


inputCreate :: DisplayServer -> Ptr WlrOutputLayout -> Ptr Backend -> IO Input
inputCreate display layout backend = do
    theme <- loadCursorTheme "default" 16
    xcursor <- getCursor theme "left_ptr"
    seat <- createSeat display "seat0"
    cursor <- createCursor

    setSeatCapabilities seat [seatCapabilityTouch, seatCapabilityKeyboard, seatCapabilityPointer]
    setXCursor cursor xcursor
    attachOutputLayout cursor layout

    let signals = backendGetSignals backend

    pure Input
        { inputCursorTheme = theme
        , inputXCursor = xcursor
        , inputCursor = cursor
        , inputSeat = seat
        }
