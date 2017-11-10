module Graphics.Wayland.WlRoots.Seat
    ( WlrSeat
    , createSeat
    , destroySeat
    , handleForClient
    , setSeatCapabilities

    , pointerNotifyEnter
    , pointerNotifyMotion

    , keyboardNotifyEnter

    , pointerClearFocus
    , pointerNotifyButton

    , keyboardNotifyKey
    , keyboardNotifyModifiers
    , seatSetKeyboard
    )
where

#include <wlr/types/wlr_seat.h>

import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt(..))
import Data.Bits ((.|.))
import Graphics.Wayland.Server (DisplayServer(..), Client (..), SeatCapability(..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons
import Graphics.Wayland.WlRoots.Input.Keyboard (KeyState(..), keyStateToInt)

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


foreign import ccall "wlr_seat_keyboard_notify_enter" c_keyboard_enter :: Ptr WlrSeat -> Ptr WlrSurface -> IO ()

keyboardNotifyEnter :: Ptr WlrSeat -> Ptr WlrSurface -> IO ()
keyboardNotifyEnter = c_keyboard_enter


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


foreign import ccall "wlr_seat_keyboard_notify_key" c_notify_key :: Ptr WlrSeat -> Word32 -> Word32 -> Word32 -> IO ()

keyboardNotifyKey :: Ptr WlrSeat -> Word32 -> Word32 -> KeyState -> IO ()
keyboardNotifyKey seat time key state = c_notify_key seat time key (keyStateToInt state)

foreign import ccall "wlr_seat_keyboard_notify_modifiers" c_notify_modifiers :: Ptr WlrSeat -> IO ()

keyboardNotifyModifiers :: Ptr WlrSeat -> IO ()
keyboardNotifyModifiers = c_notify_modifiers

foreign import ccall unsafe "wlr_seat_set_keyboard" c_set_keyboard :: Ptr WlrSeat  -> Ptr InputDevice -> IO ()

seatSetKeyboard :: Ptr WlrSeat -> Ptr InputDevice -> IO ()
seatSetKeyboard = c_set_keyboard
