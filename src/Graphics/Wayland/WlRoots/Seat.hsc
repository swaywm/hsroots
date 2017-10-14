module Graphics.Wayland.WlRoots.Seat
    ( WlrSeat
    , createSeat
    , destroySeat
    , handleForClient
    , setSeatCapabilities
    )
where

#include <wlr/types/wlr_seat.h>

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, withCString)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt(..))
import Data.Bits ((.|.))
import Graphics.Wayland.Server (DisplayServer(..), Client (..), SeatCapability(..))

data WlrSeat

foreign import ccall "wlr_seat_create" c_create :: Ptr DisplayServer -> CString -> IO (Ptr WlrSeat)

createSeat :: DisplayServer -> String -> IO (Ptr WlrSeat)
createSeat (DisplayServer ptr) name = throwErrnoIfNull "createSeat" $ withCString name $ c_create ptr


foreign import ccall "wlr_seat_destroy" c_destroy :: Ptr WlrSeat -> IO ()

destroySeat :: Ptr WlrSeat -> IO ()
destroySeat = c_destroy


data WlrSeatHandle

foreign import ccall "wlr_seat_handle_for_client" c_handle_for_client :: Ptr WlrSeat -> Ptr Client -> IO (Ptr WlrSeatHandle)

handleForClient :: Ptr WlrSeat -> Client -> IO (Ptr WlrSeatHandle)
handleForClient seat (Client client) =
    throwErrnoIfNull "handleForClient" $ c_handle_for_client seat client


foreign import ccall "wlr_seat_set_capabilities" c_set_caps :: Ptr WlrSeat -> CInt -> IO ()

setSeatCapabilities :: Ptr WlrSeat -> [SeatCapability] -> IO ()
setSeatCapabilities seat xs =
    c_set_caps seat (fromIntegral $ foldr ((.|.) . unCap) 0 xs)
    where unCap :: SeatCapability -> Int
          unCap (SeatCapability x) = x
