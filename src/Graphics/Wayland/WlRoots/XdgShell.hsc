{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.XdgShell
    ( WlrXdgShell
    , xdgShellCreate

    , WlrXdgSurface
    , xdgShellGetSurfaces
    , xdgSurfaceGetSurface
    )
where

#include <wlr/types/wlr_xdg_shell_v6.h>

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.Error (throwErrnoIfNull)

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.List (getListFromHead)
import Graphics.Wayland.Resource (getUserData)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)


data WlrXdgShell

foreign import ccall unsafe "wlr_xdg_shell_v6_create" c_shell_create :: Ptr DisplayServer -> IO (Ptr WlrXdgShell)

xdgShellCreate :: DisplayServer -> IO (Ptr WlrXdgShell)
xdgShellCreate (DisplayServer ptr) =
    throwErrnoIfNull "shellCreate" $ c_shell_create ptr


data WlrXdgSurface

xdgShellGetSurfaces :: Ptr WlrXdgShell -> IO [Ptr WlrXdgSurface]
xdgShellGetSurfaces shell = pure []
--    let list = #{ptr struct wlr_xdg_shell_v6, surfaces} shell
--     in getListFromHead list #{offset struct wlr_xdg_surface_v6, link}

xdgSurfaceGetSurface :: Ptr WlrXdgSurface -> IO (Ptr WlrSurface)
xdgSurfaceGetSurface =
    fmap getUserData . #{peek struct wlr_xdg_surface_v6, surface}
