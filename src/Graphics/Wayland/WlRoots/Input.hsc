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

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt)
import Foreign.Storable (Storable(..))

data DeviceType
    = DeviceKeyboard
    | DevicePointer
    | DeviceTouch
    | DeviceTabletTool
    | DeviceTabletPad
    deriving (Eq, Show, Read)

deviceTypeToInt :: Num a => DeviceType -> a
deviceTypeToInt DeviceKeyboard   = #{const WLR_INPUT_DEVICE_KEYBOARD}
deviceTypeToInt DevicePointer    = #{const WLR_INPUT_DEVICE_POINTER}
deviceTypeToInt DeviceTouch      = #{const WLR_INPUT_DEVICE_TOUCH}
deviceTypeToInt DeviceTabletTool = #{const WLR_INPUT_DEVICE_TABLET_TOOL}
deviceTypeToInt DeviceTabletPad  = #{const WLR_INPUT_DEVICE_TABLET_PAD}

intToDeviceType :: (Eq a, Num a, Show a) => a -> DeviceType
intToDeviceType #{const WLR_INPUT_DEVICE_KEYBOARD}    = DeviceKeyboard
intToDeviceType #{const WLR_INPUT_DEVICE_POINTER}     = DevicePointer
intToDeviceType #{const WLR_INPUT_DEVICE_TOUCH}       = DeviceTouch
intToDeviceType #{const WLR_INPUT_DEVICE_TABLET_TOOL} = DeviceTabletTool
intToDeviceType #{const WLR_INPUT_DEVICE_TABLET_PAD}  = DeviceTabletPad
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
    pure $ intToDeviceType int
