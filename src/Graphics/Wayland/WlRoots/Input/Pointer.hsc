{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumDecimals #-}
module Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrPointer
    , WlrEventPointerButton (..)

    , pointerGetEvents
    , PointerEvents (..)
    )
where

#include <wlr/types/wlr_pointer.h>

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, plusPtr)
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.WlRoots.Input.Buttons
import {-# SOURCE #-} Graphics.Wayland.WlRoots.Input (InputDevice)
import Data.Word (Word32, Word64)

data PointerEvents = PointerEvents
    { pointerButton :: Ptr (WlSignal WlrEventPointerButton)

    }

pointerGetEvents :: Ptr WlrPointer -> PointerEvents
pointerGetEvents ptr = PointerEvents
    { pointerButton = #{ptr struct wlr_pointer, events.button} ptr
    }

data WlrPointer

data WlrEventPointerButton = WlrEventPointerButton
    { eventPointerButtonDevice :: Ptr InputDevice
    , eventPointerButtonTime :: Integer
    , eventPointerButtonButton :: Word32
    , eventPointerButtonState :: ButtonState
    } deriving (Show, Eq)

instance Storable WlrEventPointerButton where
    sizeOf _ = #{size struct wlr_event_pointer_button}
    alignment _ = #{alignment struct wlr_event_pointer_button}
    peek ptr = do
        dev <- #{peek struct wlr_event_pointer_button, device} ptr
        button <- #{peek struct wlr_event_pointer_button, button} ptr
        
        state :: CInt <- #{peek struct wlr_event_pointer_button, state} ptr
        tsec :: Word32 <- #{peek struct wlr_event_pointer_button, time_sec} ptr
        tusec :: Word64 <- #{peek struct wlr_event_pointer_button, time_usec} ptr

        pure $ WlrEventPointerButton 
            dev
            (fromIntegral tsec * 1e6 + fromIntegral tusec)
            button
            (intToButtonState state)
    poke ptr event = do
        #{poke struct wlr_event_pointer_button, device} ptr $ eventPointerButtonDevice event
        #{poke struct wlr_event_pointer_button, button} ptr $ eventPointerButtonButton event
        let state :: CInt = buttonStateToInt $ eventPointerButtonState event
        #{poke struct wlr_event_pointer_button, state} ptr state
        let tsec :: Word32 = fromIntegral $ eventPointerButtonTime event `div` 1e6
        let tusec :: Word64 = fromIntegral $ eventPointerButtonTime event `mod` 1e6
        #{poke struct wlr_event_pointer_button, time_sec} ptr tsec
        #{poke struct wlr_event_pointer_button, time_usec} ptr tusec
