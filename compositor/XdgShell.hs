{-# LANGUAGE ScopedTypeVariables #-}
module XdgShell
    ( getXdgSurfaces
    , xdgShellCreate
    , XdgShell
    )
where

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
import qualified Data.IntMap as M
import Data.IntMap (IntMap)

ptrToInt :: Num b => Ptr a -> b
ptrToInt = fromIntegral . ptrToIntPtr

type MapRef =  IORef (IntMap (Ptr R.WlrXdgSurface))

data XdgShell = XdgShell
    { xdgSurfaceRef :: MapRef
    , xdgWlrootsShell :: Ptr R.WlrXdgShell
    }

xdgShellCreate :: DisplayServer -> IO XdgShell
xdgShellCreate display = do
    surfaces <- newIORef mempty
    roots <- R.xdgShellCreate (handleXdgSurface surfaces) display
    pure $ XdgShell
        { xdgSurfaceRef = surfaces
        , xdgWlrootsShell = roots
        }

handleXdgDestroy :: MapRef -> Ptr R.WlrXdgSurface -> IO ()
handleXdgDestroy ref surf = do
    modifyIORef ref $ M.delete (ptrToInt surf)

    sptr :: Ptr () <- peek (R.getXdgSurfaceDataPtr surf)
    freeStablePtr $ castPtrToStablePtr sptr

handleXdgSurface :: MapRef -> Ptr R.WlrXdgSurface -> IO ()
handleXdgSurface ref surf = do
    modifyIORef ref $ M.insert (ptrToInt surf) surf

    let signals = R.getXdgSurfaceEvents surf
    handler <- addListener (WlListener $ handleXdgDestroy ref) (R.xdgSurfacEvtDestroy signals)
    sptr <- newStablePtr handler
    poke (R.getXdgSurfaceDataPtr surf) (castStablePtrToPtr sptr)

getXdgSurfaces :: XdgShell -> IO [Ptr R.WlrXdgSurface]
getXdgSurfaces = fmap M.elems . readIORef . xdgSurfaceRef
