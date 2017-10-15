module Input.Cursor
where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrEventPointerButton (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)
    )
import Graphics.Wayland.WlRoots.Cursor
    ( WlrCursor
    , createCursor
    , warpCursorAbs
    , cursorGetEvents
    , CursorEvents (..)
    , attachOutputLayout
    , moveCursor
    , mapToRegion
    )
import Graphics.Wayland.WlRoots.OutputLayout (WlrOutputLayout)
import Graphics.Wayland.Signal
    ( addListener
    , WlListener (..)
    , ListenerToken
    )


data Cursor = Cursor
    { cursorRoots :: Ptr WlrCursor
    , cursorTokens :: [ListenerToken]
    }

cursorCreate :: Ptr WlrOutputLayout -> IO Cursor
cursorCreate layout = do
    cursor <- createCursor
    attachOutputLayout cursor layout
    mapToRegion cursor Nothing

    let signal = cursorGetEvents cursor
    tokb <- addListener (WlListener $ handleCursorButton cursor) $ cursorButton signal
    tokm <- addListener (WlListener $ handleCursorMotion cursor) $ cursorMotion signal
    toka <- addListener (WlListener $ handleCursorMotionAbs cursor) $ cursorMotionAbs signal

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka]
        }


handleCursorMotion :: Ptr WlrCursor -> Ptr WlrEventPointerMotion -> IO ()
handleCursorMotion cursor event_ptr = do
    event <- peek event_ptr

    moveCursor
        cursor
        (Just $ eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)

handleCursorMotionAbs :: Ptr WlrCursor -> Ptr WlrEventPointerAbsMotion -> IO ()
handleCursorMotionAbs cursor event_ptr = do
    event <- peek event_ptr

    warpCursorAbs
        cursor
        (Just $ eventPointerAbsMotionDevice event)
        (eventPointerAbsMotionX event / eventPointerAbsMotionWidth event)
        (eventPointerAbsMotionY event / eventPointerAbsMotionHeight event)

handleCursorButton :: Ptr WlrCursor -> Ptr WlrEventPointerButton -> IO ()
handleCursorButton _ _ = pure ()
