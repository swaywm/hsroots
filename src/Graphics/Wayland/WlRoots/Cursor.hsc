module Graphics.Wayland.WlRoots.Cursor
    ( WlrCursor
    , createCursor
    , destroyCursor

    , getCursorX
    , getCursorY

    , setXCursor
    , warpCursor
    , warpCursorAbs
    , moveCursor
    , attachInputDevice
    , detachInputDevice
    , attachOutputLayout

    , mapToOutput
    , mapInputToOutput

    , CursorEvents (..)
    , cursorGetEvents
    )
where

#include <wlr/types/wlr_cursor.h>

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.C.Error (throwErrnoIfNull)
import Graphics.Wayland.WlRoots.XCursor (WlrXCursor)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Pointer (WlrEventPointerButton)
import Graphics.Wayland.WlRoots.Output (Output)
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.Signal (WlSignal)

data CursorEvents = CursorEvents
    { cursorButton :: Ptr (WlSignal WlrEventPointerButton)

    }

cursorGetEvents :: Ptr WlrCursor -> CursorEvents
cursorGetEvents ptr = CursorEvents
    { cursorButton = #{ptr struct wlr_cursor, events.button} ptr
    }

data WlrCursor

foreign import ccall "wlr_cursor_create" c_cursor_create :: IO (Ptr WlrCursor)

createCursor :: IO (Ptr WlrCursor)
createCursor = throwErrnoIfNull "createCursor" c_cursor_create

getCursorX :: Ptr WlrCursor -> IO Double
getCursorX =  #{peek struct wlr_cursor, x}

getCursorY :: Ptr WlrCursor -> IO Double
getCursorY =  #{peek struct wlr_cursor, y}


foreign import ccall "wlr_cursor_destroy" c_cursor_destroy :: Ptr WlrCursor -> IO ()

destroyCursor :: Ptr WlrCursor -> IO ()
destroyCursor = c_cursor_destroy


foreign import ccall "wlr_cursor_set_xcursor" c_set_xcursor :: Ptr WlrCursor -> Ptr WlrXCursor -> IO ()

setXCursor :: Ptr WlrCursor -> Ptr WlrXCursor -> IO ()
setXCursor = c_set_xcursor


foreign import ccall "wlr_cursor_warp" c_cursor_warp :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> IO Bool

warpCursor :: Ptr WlrCursor -> Maybe (Ptr InputDevice) -> Double -> Double -> IO Bool
warpCursor cursor Nothing x y = warpCursor cursor (Just nullPtr) x y
warpCursor cursor (Just dev) x y = c_cursor_warp cursor dev x y


foreign import ccall "wlr_cursor_warp_absolute" c_cursor_warp_abs :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> IO ()

warpCursorAbs :: Ptr WlrCursor -> Maybe (Ptr InputDevice) -> Double -> Double -> IO ()
warpCursorAbs cursor Nothing x y = warpCursorAbs cursor (Just nullPtr) x y
warpCursorAbs cursor (Just dev) x y = c_cursor_warp_abs cursor dev x y


foreign import ccall "wlr_cursor_move" c_cursor_move :: Ptr WlrCursor -> Ptr InputDevice -> Double -> Double -> IO ()

moveCursor :: Ptr WlrCursor -> Maybe (Ptr InputDevice) -> Double -> Double -> IO ()
moveCursor cursor Nothing x y = moveCursor cursor (Just nullPtr) x y
moveCursor cursor (Just dev) x y = c_cursor_move cursor dev x y


foreign import ccall "wlr_cursor_attach_input_device" c_attach_input_device :: Ptr WlrCursor -> Ptr InputDevice -> IO ()

attachInputDevice :: Ptr WlrCursor -> Ptr InputDevice -> IO ()
attachInputDevice = c_attach_input_device


foreign import ccall "wlr_cursor_detach_input_device" c_detach_input_device :: Ptr WlrCursor -> Ptr InputDevice -> IO ()

detachInputDevice :: Ptr WlrCursor -> Ptr InputDevice -> IO ()
detachInputDevice = c_detach_input_device


foreign import ccall "wlr_cursor_attach_output_layout" c_attach_layout :: Ptr WlrCursor -> Ptr WlrOutputLayout -> IO ()

attachOutputLayout :: Ptr WlrCursor -> Ptr WlrOutputLayout -> IO ()
attachOutputLayout = c_attach_layout


foreign import ccall "wlr_cursor_map_to_output" c_map_to_output :: Ptr WlrCursor -> Ptr Output -> IO ()

mapToOutput :: Ptr WlrCursor -> Ptr Output -> IO ()
mapToOutput = c_map_to_output


foreign import ccall "wlr_cursor_map_input_to_output" c_map_intput_to_output :: Ptr WlrCursor -> Ptr InputDevice -> Ptr Output -> IO ()

mapInputToOutput :: Ptr WlrCursor -> Ptr InputDevice -> Ptr Output -> IO ()
mapInputToOutput = c_map_intput_to_output


-- TODO: Box stuff
