{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumDecimals #-}
module Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrPointer
    , WlrEventPointerButton (..)

    , pointerGetEvents
    , PointerEvents (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)
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
    , pointerMotion :: Ptr (WlSignal WlrEventPointerMotion)
    , pointerMotionAbs :: Ptr (WlSignal WlrEventPointerAbsMotion)
    , pointerAxis :: Ptr (WlSignal ())
    }

pointerGetEvents :: Ptr WlrPointer -> PointerEvents
pointerGetEvents ptr = PointerEvents
    { pointerButton = #{ptr struct wlr_pointer, events.button} ptr
    , pointerMotion = #{ptr struct wlr_pointer, events.motion} ptr
    , pointerMotionAbs = #{ptr struct wlr_pointer, events.motion_absolute} ptr
    , pointerAxis = #{ptr struct wlr_pointer, events.axis} ptr
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
        tsec :: Word32 <- #{peek struct wlr_event_pointer_button, time_msec} ptr

        pure $ WlrEventPointerButton 
            dev
            (fromIntegral tsec)
            button
            (intToButtonState state)
    poke ptr event = do
        #{poke struct wlr_event_pointer_button, device} ptr $ eventPointerButtonDevice event
        #{poke struct wlr_event_pointer_button, button} ptr $ eventPointerButtonButton event
        let state :: CInt = buttonStateToInt $ eventPointerButtonState event
        #{poke struct wlr_event_pointer_button, state} ptr state
        let tsec :: Word32 = fromIntegral $ eventPointerButtonTime event
        #{poke struct wlr_event_pointer_button, time_msec} ptr tsec

data WlrEventPointerMotion = WlrEventPointerMotion
    { eventPointerMotionDevice :: Ptr InputDevice
    , eventPointerMotionTime :: Integer
    , eventPointerMotionDeltaX :: Double
    , eventPointerMotionDeltaY :: Double
    } deriving (Show, Eq)

instance Storable WlrEventPointerMotion where
    sizeOf _ = #{size struct wlr_event_pointer_motion}
    alignment _ = #{alignment struct wlr_event_pointer_motion}
    peek ptr = do
        dev <- #{peek struct wlr_event_pointer_motion, device} ptr
        tsec :: Word32 <- #{peek struct wlr_event_pointer_motion, time_msec} ptr
        deltax <- #{peek struct wlr_event_pointer_motion, delta_x} ptr
        deltay <- #{peek struct wlr_event_pointer_motion, delta_y} ptr

        pure $ WlrEventPointerMotion
            dev
            (fromIntegral tsec)
            deltax
            deltay
    poke ptr event = do
        #{poke struct wlr_event_pointer_motion, device} ptr $ eventPointerMotionDevice event
        let tsec :: Word32 = fromIntegral $ eventPointerMotionTime event
        #{poke struct wlr_event_pointer_motion, time_msec} ptr tsec

        #{poke struct wlr_event_pointer_motion, delta_x} ptr $ eventPointerMotionDeltaX event
        #{poke struct wlr_event_pointer_motion, delta_y} ptr $ eventPointerMotionDeltaY event


data WlrEventPointerAbsMotion = WlrEventPointerAbsMotion
    { eventPointerAbsMotionDevice :: Ptr InputDevice
    , eventPointerAbsMotionTime :: Integer
    , eventPointerAbsMotionX :: Double
    , eventPointerAbsMotionY :: Double

    , eventPointerAbsMotionWidth :: Double
    , eventPointerAbsMotionHeight :: Double
    } deriving (Show, Eq)

instance Storable WlrEventPointerAbsMotion where
    sizeOf _ = #{size struct wlr_event_pointer_motion_absolute}
    alignment _ = #{alignment struct wlr_event_pointer_motion_absolute}
    peek ptr = do
        dev <- #{peek struct wlr_event_pointer_motion_absolute, device} ptr
        tsec :: Word32 <- #{peek struct wlr_event_pointer_motion_absolute, time_msec} ptr
        x <- #{peek struct wlr_event_pointer_motion_absolute, x_mm} ptr
        y <- #{peek struct wlr_event_pointer_motion_absolute, y_mm} ptr

        width <- #{peek struct wlr_event_pointer_motion_absolute, width_mm} ptr
        height <- #{peek struct wlr_event_pointer_motion_absolute, height_mm} ptr

        pure $ WlrEventPointerAbsMotion
            dev
            (fromIntegral tsec)
            x
            y
            width
            height

    poke ptr event = do
        #{poke struct wlr_event_pointer_motion_absolute, device} ptr $ eventPointerAbsMotionDevice event
        let tsec :: Word32 = fromIntegral $ eventPointerAbsMotionTime event
        #{poke struct wlr_event_pointer_motion_absolute, time_msec} ptr tsec

        #{poke struct wlr_event_pointer_motion_absolute, x_mm} ptr $ eventPointerAbsMotionX event
        #{poke struct wlr_event_pointer_motion_absolute, y_mm} ptr $ eventPointerAbsMotionY event

        #{poke struct wlr_event_pointer_motion_absolute, width_mm} ptr $ eventPointerAbsMotionWidth event
        #{poke struct wlr_event_pointer_motion_absolute, height_mm} ptr $ eventPointerAbsMotionHeight event
