module Graphics.Wayland.WlRoots.Seat
    ( WlrSeat
    , createSeat
    , destroySeat
    , handleForClient
    , setSeatCapabilities

    , pointerNotifyEnter
    , pointerNotifyMotion

    , keyboardNotifyEnter
    , getSeatKeyboard

    , pointerClearFocus
    , pointerNotifyButton
    , pointerNotifyAxis

    , keyboardNotifyKey
    , keyboardNotifyModifiers
    , seatSetKeyboard

    , keyboardClearFocus

    , SetCursorEvent (..)
    , SeatSignals (..)

    , WlrSeatClient
    , seatGetSignals
    , seatClientGetClient

    , WlrSeatKeyboardState
    , getKeyboardState
    , getKeyboardFocus

    , WlrSeatPointerState
    , getPointerState
    , getPointerFocus
    )
where

#include <wlr/types/wlr_seat.h>

import Foreign.Storable(Storable(..))
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.String (CString, withCString)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt(..), CSize (..))
import Data.Bits ((.|.))
import Graphics.Wayland.Server (DisplayServer(..), Client (..), SeatCapability(..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons
import Graphics.Wayland.WlRoots.Input.Pointer (axisOToInt, AxisOrientation)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard, KeyState(..), keyStateToInt, KeyboardModifiers)

import Graphics.Wayland.Signal (WlSignal)

data WlrSeat

foreign import ccall "wlr_seat_create" c_create :: Ptr DisplayServer -> CString -> IO (Ptr WlrSeat)

createSeat :: DisplayServer -> String -> IO (Ptr WlrSeat)
createSeat (DisplayServer ptr) name = throwErrnoIfNull "createSeat" $ withCString name $ c_create ptr


foreign import ccall "wlr_seat_destroy" c_destroy :: Ptr WlrSeat -> IO ()

destroySeat :: Ptr WlrSeat -> IO ()
destroySeat = c_destroy


data WlrSeatHandle

foreign import ccall "wlr_seat_client_for_wl_client" c_handle_for_client :: Ptr WlrSeat -> Ptr Client -> IO (Ptr WlrSeatHandle)

handleForClient :: Ptr WlrSeat -> Client -> IO (Ptr WlrSeatHandle)
handleForClient seat (Client client) =
    throwErrnoIfNull "handleForClient" $ c_handle_for_client seat client


foreign import ccall "wlr_seat_set_capabilities" c_set_caps :: Ptr WlrSeat -> CInt -> IO ()

setSeatCapabilities :: Ptr WlrSeat -> [SeatCapability] -> IO ()
setSeatCapabilities seat xs =
    c_set_caps seat (fromIntegral $ foldr ((.|.) . unCap) 0 xs)
    where unCap :: SeatCapability -> Int
          unCap (SeatCapability x) = x


foreign import ccall "wlr_seat_pointer_notify_enter" c_pointer_enter :: Ptr WlrSeat -> Ptr WlrSurface -> Double -> Double -> IO ()

pointerNotifyEnter :: Ptr WlrSeat -> Ptr WlrSurface -> Double -> Double -> IO ()
pointerNotifyEnter = c_pointer_enter


foreign import ccall "wlr_seat_keyboard_notify_enter" c_keyboard_enter :: Ptr WlrSeat -> Ptr WlrSurface -> Ptr Word32 -> CSize -> Ptr KeyboardModifiers -> IO ()

keyboardNotifyEnter :: Ptr WlrSeat -> Ptr WlrSurface -> Ptr Word32 -> CSize -> Ptr KeyboardModifiers -> IO ()
keyboardNotifyEnter = c_keyboard_enter

-- struct wlr_keyboard *wlr_seat_get_keyboard(struct wlr_seat *seat);
foreign import ccall unsafe "wlr_seat_get_keyboard" c_get_keyboard :: Ptr WlrSeat -> IO (Ptr WlrKeyboard)

getSeatKeyboard :: Ptr WlrSeat -> IO (Ptr WlrKeyboard)
getSeatKeyboard = c_get_keyboard

foreign import ccall "wlr_seat_pointer_notify_motion" c_pointer_motion :: Ptr WlrSeat -> Word32 -> Double -> Double -> IO ()

pointerNotifyMotion :: Ptr WlrSeat -> Word32 -> Double -> Double -> IO ()
pointerNotifyMotion = c_pointer_motion


foreign import ccall "wlr_seat_pointer_clear_focus" c_pointer_clear_focus :: Ptr WlrSeat -> IO ()

pointerClearFocus :: Ptr WlrSeat -> IO ()
pointerClearFocus = c_pointer_clear_focus


foreign import ccall unsafe "wlr_seat_pointer_notify_button" c_notify_button :: Ptr WlrSeat -> Word32 -> Word32 -> Word32 -> IO ()

pointerNotifyButton :: Ptr WlrSeat -> Word32 -> Word32 -> ButtonState -> IO ()
pointerNotifyButton seat time button state =
    c_notify_button seat time button (buttonStateToInt state)

foreign import ccall unsafe "wlr_seat_pointer_notify_axis" c_pointer_notify_axis :: Ptr WlrSeat -> Word32 -> CInt -> Double -> IO ()

pointerNotifyAxis :: Ptr WlrSeat -> Word32 -> AxisOrientation -> Double -> IO ()
pointerNotifyAxis seat time orientation value =
    c_pointer_notify_axis seat time (axisOToInt orientation) value

foreign import ccall "wlr_seat_keyboard_notify_key" c_notify_key :: Ptr WlrSeat -> Word32 -> Word32 -> Word32 -> IO ()

keyboardNotifyKey :: Ptr WlrSeat -> Word32 -> Word32 -> KeyState -> IO ()
keyboardNotifyKey seat time key state = c_notify_key seat time key (keyStateToInt state)

foreign import ccall "wlr_seat_keyboard_notify_modifiers" c_notify_modifiers :: Ptr WlrSeat -> Ptr KeyboardModifiers -> IO ()

keyboardNotifyModifiers :: Ptr WlrSeat -> Ptr KeyboardModifiers -> IO ()
keyboardNotifyModifiers = c_notify_modifiers

foreign import ccall unsafe "wlr_seat_set_keyboard" c_set_keyboard :: Ptr WlrSeat  -> Ptr InputDevice -> IO ()

seatSetKeyboard :: Ptr WlrSeat -> Ptr InputDevice -> IO ()
seatSetKeyboard = c_set_keyboard

foreign import ccall "wlr_seat_keyboard_clear_focus" c_keyboard_clear_focus :: Ptr WlrSeat -> IO ()

keyboardClearFocus :: Ptr WlrSeat -> IO ()
keyboardClearFocus = c_keyboard_clear_focus

data WlrSeatClient

seatClientGetClient :: Ptr WlrSeatClient -> IO Client
seatClientGetClient = fmap Client . #{peek struct wlr_seat_client, client}

data SetCursorEvent = SetCursorEvent
    { seatCursorSurfaceClient   :: Ptr WlrSeatClient
    , seatCursorSurfaceSurface  :: Ptr WlrSurface
    , seatCursorSurfaceSerial   :: Word32
    , seatCursorSurfaceHotspotX :: Int32
    , seatCursorSurfaceHotspotY :: Int32
    }

instance Storable SetCursorEvent where
    sizeOf _ = #{size struct wlr_seat_pointer_request_set_cursor_event}
    alignment _ = #{alignment struct wlr_seat_pointer_request_set_cursor_event}
    peek ptr = SetCursorEvent
        <$> #{peek struct wlr_seat_pointer_request_set_cursor_event, seat_client} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, surface} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, serial} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, hotspot_x} ptr
        <*> #{peek struct wlr_seat_pointer_request_set_cursor_event, hotspot_y} ptr
    poke ptr evt = do
        #{poke struct wlr_seat_pointer_request_set_cursor_event, seat_client} ptr $ seatCursorSurfaceClient evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, surface} ptr $ seatCursorSurfaceSurface evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, serial} ptr $ seatCursorSurfaceSerial evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, hotspot_x} ptr $ seatCursorSurfaceHotspotX evt
        #{poke struct wlr_seat_pointer_request_set_cursor_event, hotspot_y} ptr $ seatCursorSurfaceHotspotY evt

data SeatSignals = SeatSignals
    { seatSignalSetCursor :: Ptr (WlSignal (SetCursorEvent))
    }

seatGetSignals :: Ptr WlrSeat -> SeatSignals
seatGetSignals ptr = SeatSignals
    { seatSignalSetCursor = #{ptr struct wlr_seat, events.request_set_cursor} ptr
    }

data WlrSeatKeyboardState

getKeyboardState :: Ptr WlrSeat -> Ptr WlrSeatKeyboardState
getKeyboardState = #{ptr struct wlr_seat, keyboard_state}

getKeyboardFocus :: Ptr WlrSeatKeyboardState -> IO (Ptr WlrSurface)
getKeyboardFocus = #{peek struct wlr_seat_keyboard_state, focused_surface}

data WlrSeatPointerState

getPointerState :: Ptr WlrSeat -> Ptr WlrSeatPointerState
getPointerState = #{ptr struct wlr_seat, pointer_state}

getPointerFocus :: Ptr WlrSeatPointerState -> IO (Ptr WlrSurface)
getPointerFocus = #{peek struct wlr_seat_pointer_state, focused_surface}


