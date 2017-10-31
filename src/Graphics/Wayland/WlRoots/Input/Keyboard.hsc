{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr

    , KeyState (..)
    , EventKey (..)

    , getKeystate

    , setKeymap
    , keyStateToInt

    , KeyboardModifiers (..)
    , readModifiers
    )
where

#include <wlr/types/wlr_keyboard.h>

import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable(..))
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr, plusPtr)

import Graphics.Wayland.Signal (WlSignal)

import Text.XkbCommon.InternalTypes (CKeymap, CKeyboardState)

data WlrKeyboard

data KeyboardSignals = KeyboardSignals
    { keySignalKey :: Ptr (WlSignal EventKey)
    , keySignalModifiers :: Ptr (WlSignal ())
    }

getKeySignals :: Ptr WlrKeyboard -> KeyboardSignals
getKeySignals ptr = KeyboardSignals 
    { keySignalKey = #{ptr struct wlr_keyboard, events.key} ptr
    , keySignalModifiers = #{ptr struct wlr_keyboard, events.modifiers} ptr
    }

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

keyStateToInt :: Num a => KeyState -> a
keyStateToInt KeyReleased = #{const WLR_KEY_RELEASED}
keyStateToInt KeyPressed = #{const WLR_KEY_PRESSED}


data EventKey = EventKey
    { timeSec :: Word32
    , keyCode :: Word32
    , state :: KeyState
    }
    deriving (Show)

instance Storable EventKey where
    sizeOf _ = #{size struct wlr_event_keyboard_key}
    alignment _ = #{alignment struct wlr_event_keyboard_key}
    peek ptr = EventKey
        <$> #{peek struct wlr_event_keyboard_key, time_msec} ptr
        <*> #{peek struct wlr_event_keyboard_key, keycode} ptr
        <*> (fmap keyStateFromInt . #{peek struct wlr_event_keyboard_key, state}) ptr
    poke = error "We don't poke EventKeys"

foreign import ccall "wlr_keyboard_set_keymap" c_set_keymap :: Ptr WlrKeyboard -> Ptr CKeymap -> IO ()

setKeymap :: Ptr WlrKeyboard -> Ptr CKeymap -> IO ()
setKeymap = c_set_keymap

getKeystate :: Ptr WlrKeyboard -> IO (Ptr CKeyboardState)
getKeystate = #{peek struct wlr_keyboard, xkb_state}

data KeyboardModifiers = Modifiers
    { modDepressed :: Word32
    , modLatched :: Word32
    , modLocked :: Word32
    , modGroup :: Word32
    }

readModifiers :: Ptr WlrKeyboard -> IO KeyboardModifiers
readModifiers ptr = Modifiers
    <$> #{peek struct wlr_keyboard, modifiers.depressed} ptr
    <*> #{peek struct wlr_keyboard, modifiers.latched} ptr
    <*> #{peek struct wlr_keyboard, modifiers.locked} ptr
    <*> #{peek struct wlr_keyboard, modifiers.group} ptr
