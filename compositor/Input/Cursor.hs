{-# LANGUAGE NumDecimals #-}
module Input.Cursor
where

import System.IO
import View (getViewSurface, activateView, getViewEventSurface)
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
    tokb <- liftIO $ addListener (WlListener $ \evt -> runWayState (handleCursorButton cursor seat evt) stateRef) $ cursorButton signal
    tokm <- liftIO $ addListener (WlListener $ \evt -> runWayState (handleCursorMotion cursor seat evt)    stateRef)$ cursorMotion signal
    toka <- liftIO $ addListener (WlListener $ \evt -> runWayState (handleCursorMotionAbs cursor seat evt) stateRef)$ cursorMotionAbs signal

    pure Cursor
        { cursorRoots = cursor
        , cursorTokens = [tokb, tokm, toka]
        }

updatePosition :: Ptr WlrCursor -> Ptr WlrSeat -> Word32 -> WayState ()
updatePosition cursor seat time = do
    baseX <- liftIO $ getCursorX cursor
    baseY <- liftIO $ getCursorY cursor

    viewM <- viewBelow $ Point (floor baseX) (floor baseY)

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just view -> liftIO $ do
            evt@(surf, x, y) <- getViewEventSurface view baseX baseY
            hPutStrLn stderr $ show evt
            pointerNotifyEnter seat surf x y
            pointerNotifyMotion seat time x y
            --keyboardNotifyEnter seat surf


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

handleCursorButton :: Ptr WlrCursor -> Ptr WlrSeat -> Ptr WlrEventPointerButton -> WayState ()
handleCursorButton cursor seat event_ptr = do
    x <- liftIO $ getCursorX cursor
    y <- liftIO $ getCursorY cursor
    event <- liftIO $ peek event_ptr

    viewM <- viewBelow $ Point (floor x) (floor y)

    case viewM of
        Nothing -> liftIO $ pointerClearFocus seat
        Just view -> liftIO $ do
            let time = (fromIntegral $ eventPointerButtonTime event `mod` 1e6 `div` 1000)
            pointerNotifyButton seat time (eventPointerButtonButton event) (eventPointerButtonState event)
