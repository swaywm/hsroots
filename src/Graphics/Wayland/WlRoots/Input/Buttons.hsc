{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input.Buttons
    ( ButtonState(..)
    , buttonStateToInt
    , intToButtonState
    )
where

import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

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

instance Storable ButtonState where
    sizeOf _ = #{size int}
    alignment _ = #{alignment int}
    peek = fmap (intToButtonState :: CInt -> ButtonState) . peek . castPtr
    poke ptr val = poke (castPtr ptr) (buttonStateToInt val :: CInt)
