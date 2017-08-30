{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Input.Keyboard
    ( WlrKeyboard
    , KeyboardSignals (..)
    , getKeySignals
    , getKeyDataPtr
    )
where

#include <wlr/types/wlr_keyboard.h>

import Foreign.Ptr (Ptr, plusPtr)

import Graphics.Wayland.Signal (WlSignal)

data WlrKeyboard

data KeyboardSignals = KeyboardSignals
    { keySignalKey :: Ptr (WlSignal ())

    }

getKeySignals :: Ptr WlrKeyboard -> KeyboardSignals
getKeySignals = KeyboardSignals . #{ptr struct wlr_keyboard, events.key}

data KeyState
    = KeyReleased
    | KeyPressed
    deriving (Show, Eq)

getKeyDataPtr :: Ptr WlrKeyboard -> Ptr (Ptr a)
getKeyDataPtr = #{ptr struct wlr_keyboard, data}
