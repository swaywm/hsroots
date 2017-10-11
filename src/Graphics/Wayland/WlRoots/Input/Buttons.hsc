module Graphics.Wayland.WlRoots.Input.Buttons
    ( ButtonState(..)
    , buttonStateToInt
    , intToButtonState
    )
where

#include <wlr/types/wlr_input_device.h>

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
