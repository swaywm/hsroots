{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.XdgShell
    ( WlrXdgShell
    , xdgShellCreate

    , WlrXdgSurface
    , xdgSurfaceGetSurface

    , WlrXdgSurfaceEvents (..)
    , getXdgSurfaceEvents
    , getXdgSurfaceDataPtr

    , sendClose
    , setSize
    , getGeometry
    , setActivated
    , setMaximized
    )
where

#include <wlr/types/wlr_xdg_shell_v6.h>

import Data.Word (Word32)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.Types (CInt)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    )

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Box (WlrBox)

import Graphics.Wayland.Signal
import Control.Monad (when)


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


foreign import ccall "wlr_xdg_toplevel_v6_send_close" c_close :: Ptr WlrXdgSurface -> IO ()

sendClose :: Ptr WlrXdgSurface -> IO ()
sendClose surf = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    when
        (role == #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL})
        (c_close surf)


getGeometry :: Ptr WlrXdgSurface -> IO WlrBox
getGeometry ptr = peek =<< #{peek struct wlr_xdg_surface_v6, geometry} ptr


foreign import ccall "wlr_xdg_toplevel_v6_set_size" c_set_size :: Ptr WlrXdgSurface -> Word32 -> Word32 -> IO ()

setSize :: Ptr WlrXdgSurface -> Word32 -> Word32 -> IO ()
setSize surf width height = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    when
        (role == #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL})
        (c_set_size surf width height)



foreign import ccall "wlr_xdg_toplevel_v6_set_activated" c_activate :: Ptr WlrXdgSurface -> Bool -> IO ()

setActivated :: Ptr WlrXdgSurface -> Bool -> IO ()
setActivated surf active = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    when
        (role == #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL})
        (c_activate surf active)



foreign import ccall "wlr_xdg_toplevel_v6_set_maximized" c_maximize :: Ptr WlrXdgSurface -> Bool -> IO ()

setMaximized :: Ptr WlrXdgSurface -> Bool -> IO ()
setMaximized surf maximized = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    when
        (role == #{const WLR_XDG_SURFACE_V6_ROLE_TOPLEVEL})
        (c_maximize surf maximized)
