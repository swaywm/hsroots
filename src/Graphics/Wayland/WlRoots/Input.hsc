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
    )
where

#include <wlr/types/wlr_input_device.h>

import Foreign.Ptr (Ptr, castPtr)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard)

data DeviceType
    = DeviceKeyboard (Ptr WlrKeyboard)
    | DevicePointer (Ptr ())
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

data ButtonState
    = ButtonReleased
    | ButtonPressed
    deriving (Eq, Show, Read)

buttonStateToInt :: Num a => ButtonState -> a
buttonStateToInt ButtonReleased = #{const WLR_BUTTON_RELEASED}
buttonStateToInt ButtonPressed = #{const WLR_BUTTON_PRESSED}

intToButtonState :: (Eq a, Num a, Show a) => a -> ButtonState
intToButtonState #{const WLR_BUTTON_RELEASED} = ButtonReleased
intToButtonState #{const WLR_BUTTON_PRESSED}  = ButtonPressed
intToButtonState x = error $ "Got an an unknown ButtonState: " ++ show x


data InputDevice

inputDeviceType :: Ptr InputDevice -> IO DeviceType
inputDeviceType ptr = do
    int :: CInt <- #{peek struct wlr_input_device, type} ptr
    devptr <- #{peek struct wlr_input_device, _device} ptr
    pure $ intToDeviceType int devptr
