{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input
    ( DeviceType(..)
    , deviceTypeToInt
    , intToDeviceType

    , ButtonState(..)
    , buttonStateToInt
    , intToButtonState

    , InputDevice
    , inputDeviceType
    , getDestroySignal
    )
where

#include <wlr/types/wlr_input_device.h>

import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.Signal (WlSignal)

import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard)
import Graphics.Wayland.WlRoots.Input.Pointer (WlrPointer)
import Graphics.Wayland.WlRoots.Input.Buttons


data DeviceType
    = DeviceKeyboard (Ptr WlrKeyboard)
    | DevicePointer (Ptr WlrPointer)
    | DeviceTouch (Ptr ())
    | DeviceTabletTool (Ptr ())
    | DeviceTabletPad (Ptr ())
    deriving (Eq, Show)

deviceTypeToInt :: Num a => DeviceType -> a
deviceTypeToInt (DeviceKeyboard   _) = #{const WLR_INPUT_DEVICE_KEYBOARD}
deviceTypeToInt (DevicePointer    _) = #{const WLR_INPUT_DEVICE_POINTER}
deviceTypeToInt (DeviceTouch      _) = #{const WLR_INPUT_DEVICE_TOUCH}
deviceTypeToInt (DeviceTabletTool _) = #{const WLR_INPUT_DEVICE_TABLET_TOOL}
deviceTypeToInt (DeviceTabletPad  _) = #{const WLR_INPUT_DEVICE_TABLET_PAD}

intToDeviceType :: (Eq a, Num a, Show a) => a -> Ptr b -> DeviceType
intToDeviceType #{const WLR_INPUT_DEVICE_KEYBOARD}    = DeviceKeyboard . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_POINTER}     = DevicePointer . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_TOUCH}       = DeviceTouch . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_TABLET_TOOL} = DeviceTabletTool . castPtr
intToDeviceType #{const WLR_INPUT_DEVICE_TABLET_PAD}  = DeviceTabletPad . castPtr
intToDeviceType x = error $ "Got an unknown DeviceType: " ++ show x

data InputDevice

inputDeviceType :: Ptr InputDevice -> IO DeviceType
inputDeviceType ptr = do
    int :: CInt <- #{peek struct wlr_input_device, type} ptr
    devptr <- #{peek struct wlr_input_device, _device} ptr
    pure $ intToDeviceType int devptr

getDestroySignal :: Ptr InputDevice -> Ptr (WlSignal (InputDevice))
getDestroySignal = #{ptr struct wlr_input_device, events.destroy}
