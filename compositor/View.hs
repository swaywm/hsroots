{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
module View
    ( ShellSurface (..)
    , View (..)
    , getViewBox
    , createView
    , moveView
    , resizeView
    , getViewSurface
    , activateView
    , renderViewAdditional
    , getViewEventSurface
    )
where

import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Control.Monad.IO.Class
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Box (WlrBox(..))
import Graphics.Wayland.WlRoots.Output (Output)

class ShellSurface a where
    getSurface :: MonadIO m => a -> m (Ptr WlrSurface)
    getSize :: MonadIO m => a -> m (Double, Double)
    resize :: MonadIO m => a -> Word32 -> Word32 -> m ()
    activate :: MonadIO m => a -> Bool -> m ()
    close :: MonadIO m => a -> m ()
    renderAdditional :: MonadIO m => (Ptr WlrSurface -> Int -> Int -> m ()) -> a -> Int -> Int -> m ()
    getEventSurface :: MonadIO m => a -> Double -> Double -> m (Ptr WlrSurface, Double, Double)

data View = forall a. ShellSurface a => View
    { viewX :: IORef Double
    , viewY :: IORef Double
    , viewSurface :: a
    }

getViewBox :: MonadIO m => View -> m WlrBox
getViewBox view = case view of
    (View xref yref surf) -> do
        (width, height) <- getSize surf
        x <- liftIO $ readIORef xref
        y <- liftIO $ readIORef yref
        pure WlrBox
            { boxX = floor x
            , boxY = floor y
            , boxWidth  = floor width
            , boxHeight = floor height
            }

createView :: (ShellSurface a, MonadIO m) => a -> m View
createView surf = do
    xref <- liftIO $ newIORef 0
    yref <- liftIO $ newIORef 0
    pure View
        { viewX = xref
        , viewY = yref
        , viewSurface = surf
        }


moveView :: MonadIO m => View -> Double -> Double -> m ()
moveView view x y = do
    liftIO $ writeIORef (viewX view) x
    liftIO $ writeIORef (viewY view) y


resizeView :: MonadIO m => View -> Double -> Double -> m ()
resizeView view width height = case view of
    (View _ _ surf) -> do
        resize surf (floor width) (floor height)


getViewSurface :: MonadIO m => View -> m (Ptr WlrSurface)
getViewSurface view = case view of
    (View _ _ surf) -> do
        getSurface surf

activateView :: MonadIO m => View -> Bool -> m ()
activateView view active = case view of
    (View _ _ surf) -> do
        activate surf active

renderViewAdditional :: MonadIO m => (Ptr WlrSurface -> Int -> Int -> m ()) -> View -> m ()
renderViewAdditional fun (View xref yref surf) = do
    x <- liftIO $ readIORef xref
    y <- liftIO $ readIORef yref
    renderAdditional fun surf (floor x) (floor y)

getViewEventSurface :: MonadIO m => View -> Double -> Double -> m (Ptr WlrSurface, Double, Double)
getViewEventSurface (View xref yref surf) x y = do
    viewX <- liftIO $ readIORef xref
    viewY <- liftIO $ readIORef yref
    getEventSurface surf (x - viewX) (y - viewY)
