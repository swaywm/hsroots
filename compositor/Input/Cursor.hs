{-# LANGUAGE NumDecimals #-}
module Input.Cursor
where

import System.IO
import View (getViewSurface, activateView)
import Data.Word (Word32)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Waymonad
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.WlRoots.Box
import Graphics.Wayland.WlRoots.Input.Pointer
    ( WlrEventPointerButton (..)
    , WlrEventPointerMotion (..)
    , WlrEventPointerAbsMotion (..)
    )
import Graphics.Wayland.WlRoots.Seat
import Graphics.Wayland.WlRoots.Cursor
    ( WlrCursor
    , createCursor
    , warpCursorAbs
    , cursorGetEvents
    , CursorEvents (..)
    , attachOutputLayout
    , moveCursor
    , mapToRegion
    , getCursorX
    , getCursorY
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

cursorCreate :: Ptr WlrOutputLayout -> Ptr WlrSeat -> WayState Cursor
cursorCreate layout seat = do
    cursor <- liftIO $ createCursor
    stateRef <- ask
    liftIO $ attachOutputLayout cursor layout
    liftIO $ mapToRegion cursor Nothing

    let signal = cursorGetEvents cursor
    tokb <- liftIO $ addListener (WlListener $ handleCursorButton cursor) $ cursorButton signal
    tokm <- liftIO $ addListener (WlListener $ \evt -> runWayState (handleCursorMotion cursor seat evt)    stateRef)$ cursorMotion signal
    toka <- liftIO $ addListener (WlListener $ \evt -> runWayState (handleCursorMotionAbs cursor seat evt) stateRef)$ cursorMotionAbs signal

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka]
        }

updatePosition :: Ptr WlrCursor -> Ptr WlrSeat -> Word32 -> WayState ()
updatePosition cursor seat time = do
    x <- liftIO $ getCursorX cursor
    y <- liftIO $ getCursorY cursor
    viewM <- viewBelow $ Point (floor x) (floor y)

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just view -> liftIO $ do
            surf <- getViewSurface view
            pointerNotifyEnter seat surf x y
            pointerNotifyMotion seat time x y
            keyboardNotifyEnter seat surf
            --activateView view True


handleCursorMotion :: Ptr WlrCursor -> Ptr WlrSeat -> Ptr WlrEventPointerMotion -> WayState ()
handleCursorMotion cursor seat event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ moveCursor
        cursor
        (Just $ eventPointerMotionDevice event)
        (eventPointerMotionDeltaX event)
        (eventPointerMotionDeltaY event)
    updatePosition cursor seat (fromIntegral $ eventPointerMotionTime event `mod` 1e6 `div` 1000)

handleCursorMotionAbs :: Ptr WlrCursor -> Ptr WlrSeat -> Ptr WlrEventPointerAbsMotion -> WayState ()
handleCursorMotionAbs cursor seat event_ptr = do
    event <- liftIO $ peek event_ptr

    liftIO $ warpCursorAbs
        cursor
        (Just $ eventPointerAbsMotionDevice event)
        (eventPointerAbsMotionX event / eventPointerAbsMotionWidth event)
        (eventPointerAbsMotionY event / eventPointerAbsMotionHeight event)
    updatePosition cursor seat (fromIntegral $ eventPointerAbsMotionTime event `mod` 1e6 `div` 1000)

handleCursorButton :: Ptr WlrCursor -> Ptr WlrEventPointerButton -> IO ()
handleCursorButton _ _ = pure ()
