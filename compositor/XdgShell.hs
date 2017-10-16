{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module XdgShell
    ( getXdgSurfaces
    , xdgShellCreate
    , XdgShell
    )
where

import View
import Waymonad
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.Composition ((.:))

import Graphics.Wayland.WlRoots.Box (WlrBox (..))
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, ptrToIntPtr)
import qualified Graphics.Wayland.WlRoots.XdgShell as R
import Data.IORef (newIORef, IORef, modifyIORef, readIORef)
import Graphics.Wayland.Server (DisplayServer)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    , freeStablePtr
    , castPtrToStablePtr
    )
import Graphics.Wayland.Signal (WlListener(..), addListener)
import qualified Data.IntMap.Strict as M
import Data.IntMap (IntMap)

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

newtype XdgSurface = XdgSurface { unXdg :: (Ptr R.WlrXdgSurface) }

type MapRef =  IORef (IntMap XdgSurface)

data XdgShell = XdgShell
    { xdgSurfaceRef :: MapRef
    , xdgWlrootsShell :: Ptr R.WlrXdgShell
    }

xdgShellCreate :: DisplayServer -> (Int -> View -> WayState ()) -> (Int -> WayState ()) -> WayState XdgShell
xdgShellCreate display addFun delFun = do
    surfaces <- liftIO $ newIORef mempty
    stateRef <- ask
    roots <- liftIO $ R.xdgShellCreate (handleXdgSurface stateRef surfaces addFun delFun) display
    pure $ XdgShell
        { xdgSurfaceRef = surfaces
        , xdgWlrootsShell = roots
        }

handleXdgDestroy :: WayStateRef -> MapRef -> (Int -> WayState ()) -> Ptr R.WlrXdgSurface -> IO ()
handleXdgDestroy stateRef ref delFun surf = do
    modifyIORef ref $ M.delete (ptrToInt surf)
    runWayState (delFun (ptrToInt surf)) stateRef

    sptr :: Ptr () <- peek (R.getXdgSurfaceDataPtr surf)
    freeStablePtr $ castPtrToStablePtr sptr


handleXdgSurface :: WayStateRef -> MapRef -> (Int -> View -> WayState ()) -> (Int -> WayState ()) -> Ptr R.WlrXdgSurface -> IO ()
handleXdgSurface stateRef ref addFun delFun surf = do
    let xdgSurf = XdgSurface surf
    modifyIORef ref $ M.insert (ptrToInt surf) xdgSurf
    view <- createView xdgSurf
    runWayState (addFun (ptrToInt surf) view) stateRef
    activate xdgSurf True
    R.setMaximized surf True

    let signals = R.getXdgSurfaceEvents surf
    handler <- addListener (WlListener $ handleXdgDestroy stateRef ref delFun) (R.xdgSurfacEvtDestroy signals)
    sptr <- newStablePtr handler
    poke (R.getXdgSurfaceDataPtr surf) (castStablePtrToPtr sptr)


getXdgSurfaces :: XdgShell -> IO [Ptr R.WlrXdgSurface]
getXdgSurfaces = fmap (fmap unXdg . M.elems) . readIORef . xdgSurfaceRef


instance ShellSurface XdgSurface where
    close = liftIO . R.sendClose . unXdg
    getSurface = liftIO . R.xdgSurfaceGetSurface . unXdg
    getSize (XdgSurface surf) = liftIO $ do
        box <- R.getGeometry surf
        pure (fromIntegral $ boxWidth box, fromIntegral $ boxHeight box)
    resize (XdgSurface surf) width height =
        liftIO $ R.setSize surf width height
    activate = liftIO .: R.setActivated . unXdg
