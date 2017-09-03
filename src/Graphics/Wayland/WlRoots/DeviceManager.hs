{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.DeviceManager
    ( WlrDeviceManager
    , managerCreate
    )
where

import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfNull)

import Graphics.Wayland.Server (DisplayServer(..))


data WlrDeviceManager

foreign import ccall unsafe "wlr_data_device_manager_create" c_manager_create :: Ptr DisplayServer -> IO (Ptr WlrDeviceManager)

managerCreate :: DisplayServer -> IO (Ptr WlrDeviceManager)
managerCreate (DisplayServer ptr) =
    throwErrnoIfNull "managerCreate" $ c_manager_create ptr


