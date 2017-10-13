{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.XdgShell
    ( WlrXdgShell
    , xdgShellCreate

    , WlrXdgSurface
    , xdgSurfaceGetSurface

    , WlrXdgSurfaceEvents (..)
    , getXdgSurfaceEvents
    , getXdgSurfaceDataPtr
    )
where

#include <wlr/types/wlr_xdg_shell_v6.h>

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    )

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)

import Graphics.Wayland.Signal


data WlrXdgShell

foreign import ccall unsafe "wlr_xdg_shell_v6_create" c_shell_create :: Ptr DisplayServer -> IO (Ptr WlrXdgShell)

xdgShellCreate :: (Ptr WlrXdgSurface -> IO ()) -> DisplayServer -> IO (Ptr WlrXdgShell)
xdgShellCreate new (DisplayServer ptr) = do
    shell <- throwErrnoIfNull "shellCreate" $ c_shell_create ptr

    let signal = #{ptr struct wlr_xdg_shell_v6, events.new_surface} shell
    handler <- addListener (WlListener new) signal
    sptr <- newStablePtr handler
    poke (#{ptr struct wlr_xdg_shell_v6, data} shell) (castStablePtrToPtr sptr)

    pure shell


data WlrXdgSurface

data WlrXdgSurfaceEvents = WlrXdgSurfaceEvents
    { xdgSurfacEvtDestroy :: Ptr (WlSignal WlrXdgSurface)

    }

xdgSurfaceGetSurface :: Ptr WlrXdgSurface -> IO (Ptr WlrSurface)
xdgSurfaceGetSurface =
    #{peek struct wlr_xdg_surface_v6, surface}


getXdgSurfaceEvents :: Ptr WlrXdgSurface -> WlrXdgSurfaceEvents
getXdgSurfaceEvents ptr = WlrXdgSurfaceEvents
    { xdgSurfacEvtDestroy = #{ptr struct wlr_xdg_surface_v6, events.destroy} ptr

    }

getXdgSurfaceDataPtr :: Ptr WlrXdgSurface -> Ptr (Ptr ())
getXdgSurfaceDataPtr = #{ptr struct wlr_xdg_surface_v6, data}
