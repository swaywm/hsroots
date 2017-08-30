{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr

    , KeyState (..)
    , EventKey (..)
    )
where

#include <wlr/types/wlr_keyboard.h>

import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable(..))
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr, plusPtr)

import Graphics.Wayland.Signal (WlSignal)

data WlrKeyboard

data KeyboardSignals = KeyboardSignals
    { keySignalKey :: Ptr (WlSignal EventKey)

    }

getKeySignals :: Ptr WlrKeyboard -> KeyboardSignals
getKeySignals = KeyboardSignals . #{ptr struct wlr_keyboard, events.key}

data KeyState
    = KeyReleased
    | KeyPressed
    deriving (Show, Eq)

getKeyDataPtr :: Ptr WlrKeyboard -> Ptr (Ptr a)
getKeyDataPtr = #{ptr struct wlr_keyboard, data}

keyStateFromInt :: CInt -> KeyState
keyStateFromInt #{const WLR_KEY_RELEASED} = KeyReleased
keyStateFromInt #{const WLR_KEY_PRESSED} = KeyPressed
keyStateFromInt x = error $ "Got invalid KeyState: " ++ show x

data EventKey = EventKey
    { timeSec :: Word32
    , timeUSec :: Word64
    , keyCode :: Word32
    , state :: KeyState
    }
    deriving (Show)

instance Storable EventKey where
    sizeOf _ = #{size struct wlr_event_keyboard_key}
    alignment _ = #{alignment struct wlr_event_keyboard_key}
    peek ptr = EventKey
        <$> #{peek struct wlr_event_keyboard_key, time_sec} ptr
        <*> #{peek struct wlr_event_keyboard_key, time_usec} ptr
        <*> #{peek struct wlr_event_keyboard_key, keycode} ptr
        <*> (fmap keyStateFromInt . #{peek struct wlr_event_keyboard_key, state}) ptr
    poke = error "We don't poke EventKeys"
