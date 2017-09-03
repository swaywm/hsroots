{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Shell
    ( WlrShell
    , shellCreate

    , WlrShellSurface
    , getShellSurfaces
    , shellSurfaceGetSurface
    )
where

#include <wlr/types/wlr_wl_shell.h>

import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.C.Error (throwErrnoIfNull)

import Graphics.Wayland.List (getListFromHead)
import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.Resource (getUserData)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)


data WlrShell

foreign import ccall unsafe "wlr_wl_shell_create" c_shell_create :: Ptr DisplayServer -> IO (Ptr WlrShell)

shellCreate :: DisplayServer -> IO (Ptr WlrShell)
shellCreate (DisplayServer ptr) =
    throwErrnoIfNull "shellCreate" $ c_shell_create ptr

data WlrShellSurface

getShellSurfaces :: Ptr WlrShell -> IO [Ptr WlrShellSurface]
getShellSurfaces shell =
    let list = #{ptr struct wlr_wl_shell, surfaces} shell
     in getListFromHead list #{offset struct wlr_wl_shell_surface, link}

shellSurfaceGetSurface :: Ptr WlrShellSurface -> IO (Ptr WlrSurface)
shellSurfaceGetSurface =
    fmap getUserData . #{peek struct wlr_wl_shell_surface, surface}
