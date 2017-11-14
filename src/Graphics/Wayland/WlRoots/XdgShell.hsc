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

    , getPopups
    , isXdgPopup
    , isConfigured

    , getPopupGeometry
    , xdgPopupAt

    , getTitle
    , getAppId
    )
where

#include <wlr/types/wlr_xdg_shell_v6.h>

import Data.ByteString.Unsafe (unsafePackCString)
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.C.Types (CInt)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Marshal.Alloc (alloca)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    )

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.List (getListFromHead)

import Graphics.Wayland.Signal
import Control.Monad (when)

import qualified Data.Text.Encoding as E

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

isConfigured :: Ptr WlrXdgSurface -> IO Bool
isConfigured = #{peek struct wlr_xdg_surface_v6, configured}

isXdgPopup :: Ptr WlrXdgSurface -> IO Bool
isXdgPopup surf = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    pure (role == #{const WLR_XDG_SURFACE_V6_ROLE_POPUP})

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

foreign import ccall "wlr_xdg_surface_v6_popup_at" c_popup_at :: Ptr WlrXdgSurface -> Double -> Double -> Ptr Double -> Ptr Double -> IO (Ptr WlrXdgSurface)

xdgPopupAt :: Ptr WlrXdgSurface -> Double -> Double -> IO (Maybe (Ptr WlrXdgSurface, Double, Double))
xdgPopupAt surf x y = alloca $ \xptr -> alloca $ \yptr -> do
    popup <- c_popup_at surf x y xptr yptr
    if popup == nullPtr
        then pure Nothing
        else do
            newx <- peek xptr
            newy <- peek yptr
            pure $ Just (popup, newx, newy)


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


getPopups :: Ptr WlrXdgSurface -> IO [Ptr WlrXdgSurface]
getPopups surf = do
    let list = #{ptr struct wlr_xdg_surface_v6, popups} surf
    getListFromHead list #{offset struct wlr_xdg_surface_v6, popup_link}

data WlrXdgPopupState

getPopupState :: Ptr WlrXdgSurface -> IO (Ptr WlrXdgPopupState)
getPopupState = #{peek struct wlr_xdg_surface_v6, popup_state}

getPopupGeometry :: Ptr WlrXdgSurface -> IO WlrBox
getPopupGeometry surf = #{peek struct wlr_xdg_popup_v6, geometry} =<< getPopupState surf


getTitle :: Ptr WlrXdgSurface -> IO Text
getTitle ptr = fmap E.decodeUtf8 . unsafePackCString =<< #{peek struct wlr_xdg_surface_v6, title} ptr

getAppId :: Ptr WlrXdgSurface -> IO Text
getAppId ptr = fmap E.decodeUtf8 . unsafePackCString =<< #{peek struct wlr_xdg_surface_v6, app_id} ptr
