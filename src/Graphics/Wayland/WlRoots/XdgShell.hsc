{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.XdgShell
    ( WlrXdgShell
    , xdgShellCreate
    , xdgShellDestroy

    , WlrXdgSurface
    , xdgSurfaceGetSurface

    , MoveEvent (..)
    , ResizeEvent (..)
    , MenuEvent (..)
    , FullscreenEvent (..)
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
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Box (WlrBox)
import Graphics.Wayland.WlRoots.Seat (WlrSeatClient)
import Graphics.Wayland.List (getListFromHead)

import Graphics.Wayland.Signal
import Utility (textFromNull)
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

foreign import ccall unsafe "wlr_xdg_shell_v6_destroy" c_shell_destroy :: Ptr WlrXdgShell -> IO ()

xdgShellDestroy :: Ptr WlrXdgShell -> IO ()
xdgShellDestroy = c_shell_destroy

data WlrXdgSurface

isConfigured :: Ptr WlrXdgSurface -> IO Bool
isConfigured = #{peek struct wlr_xdg_surface_v6, configured}

isXdgPopup :: Ptr WlrXdgSurface -> IO Bool
isXdgPopup surf = do
    role :: CInt <- #{peek struct wlr_xdg_surface_v6, role} surf
    pure (role == #{const WLR_XDG_SURFACE_V6_ROLE_POPUP})

xdgSurfaceGetSurface :: Ptr WlrXdgSurface -> IO (Maybe (Ptr WlrSurface))
xdgSurfaceGetSurface ptr = do
    ret <- #{peek struct wlr_xdg_surface_v6, surface} ptr
    pure $ if ret == nullPtr
        then Nothing
        else Just ret

data MoveEvent = MoveEvent
    { moveEvtSurface :: Ptr WlrXdgSurface
    , moveEvtSeat    :: Ptr WlrSeatClient
    , moveEvtSerial  :: Word32
    }

instance Storable MoveEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_move_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_move_event}
    peek ptr = MoveEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_move_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_move_event, seat} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_move_event, serial} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_move_event, surface} ptr $ moveEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_move_event, seat} ptr $ moveEvtSeat evt
        #{poke struct wlr_xdg_toplevel_v6_move_event, serial} ptr $ moveEvtSerial evt


data ResizeEvent = ResizeEvent
    { resizeEvtSurface :: Ptr WlrXdgSurface
    , resizeEvtSeat    :: Ptr WlrSeatClient
    , resizeEvtSerial  :: Word32
    , resizeEvtEdges   :: Word32 -- TODO: Make this a [Edge]
    }

instance Storable ResizeEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_resize_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_resize_event}
    peek ptr = ResizeEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_resize_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_resize_event, seat} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_resize_event, serial} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_resize_event, edges} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_resize_event, surface} ptr $ resizeEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_resize_event, seat} ptr $ resizeEvtSeat evt
        #{poke struct wlr_xdg_toplevel_v6_resize_event, serial} ptr $ resizeEvtSerial evt
        #{poke struct wlr_xdg_toplevel_v6_resize_event, edges} ptr $ resizeEvtEdges evt

data MenuEvent = MenuEvent
    { menuEvtSurface :: Ptr WlrXdgSurface
    , menuEvtSeat    :: Ptr WlrSeatClient
    , menuEvtSerial  :: Word32
    , menuEvtX       :: Word32
    , menuEvtY       :: Word32
    }

instance Storable MenuEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_show_window_menu_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_show_window_menu_event}
    peek ptr = MenuEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, seat} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, serial} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, x} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_show_window_menu_event, y} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, surface} ptr $ menuEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, seat} ptr $ menuEvtSeat evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, serial} ptr $ menuEvtSerial evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, x} ptr $ menuEvtX evt
        #{poke struct wlr_xdg_toplevel_v6_show_window_menu_event, y} ptr $ menuEvtY evt

data FullscreenEvent = FullscreenEvent
    { fullscreenEvtSurface :: Ptr WlrXdgSurface
    , fullscreenEvtFull    :: Bool
    , fullscreenEvtOutput  :: Ptr WlrOutput
    }

instance Storable FullscreenEvent where
    sizeOf _ = #{size struct wlr_xdg_toplevel_v6_set_fullscreen_event}
    alignment _ = #{alignment struct wlr_xdg_toplevel_v6_set_fullscreen_event}
    peek ptr = FullscreenEvent
        <$> #{peek struct wlr_xdg_toplevel_v6_set_fullscreen_event, surface} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_set_fullscreen_event, fullscreen} ptr
        <*> #{peek struct wlr_xdg_toplevel_v6_set_fullscreen_event, output} ptr
    poke ptr evt = do
        #{poke struct wlr_xdg_toplevel_v6_set_fullscreen_event, surface} ptr $ fullscreenEvtSurface evt
        #{poke struct wlr_xdg_toplevel_v6_set_fullscreen_event, fullscreen} ptr $ fullscreenEvtFull evt
        #{poke struct wlr_xdg_toplevel_v6_set_fullscreen_event, output} ptr $ fullscreenEvtOutput evt

data WlrXdgSurfaceEvents = WlrXdgSurfaceEvents
    { xdgSurfaceEvtCommit  :: Ptr (WlSignal WlrXdgSurface)
    , xdgSurfaceEvtDestroy :: Ptr (WlSignal WlrXdgSurface)
    , xdgSurfaceEvtTimeout :: Ptr (WlSignal WlrXdgSurface)

    , xdgSurfaceEvtMaximize   :: Ptr (WlSignal WlrXdgSurface)
    , xdgSurfaceEvtFullscreen :: Ptr (WlSignal FullscreenEvent)
    , xdgSurfaceEvtMinimize   :: Ptr (WlSignal WlrXdgSurface)

    , xdgSurfaceEvtMove   :: Ptr (WlSignal MoveEvent)
    , xdgSurfaceEvtResize :: Ptr (WlSignal ResizeEvent)
    , xdgSurfaceEvtMenu   :: Ptr (WlSignal MenuEvent)
    }

getXdgSurfaceEvents :: Ptr WlrXdgSurface -> WlrXdgSurfaceEvents
getXdgSurfaceEvents ptr = WlrXdgSurfaceEvents
    { xdgSurfaceEvtDestroy = #{ptr struct wlr_xdg_surface_v6, events.destroy} ptr
    , xdgSurfaceEvtCommit = #{ptr struct wlr_xdg_surface_v6, events.destroy} ptr
    , xdgSurfaceEvtTimeout = #{ptr struct wlr_xdg_surface_v6, events.ping_timeout} ptr

    , xdgSurfaceEvtMaximize = #{ptr struct wlr_xdg_surface_v6, events.request_maximize} ptr
    , xdgSurfaceEvtFullscreen = #{ptr struct wlr_xdg_surface_v6, events.request_fullscreen} ptr
    , xdgSurfaceEvtMinimize = #{ptr struct wlr_xdg_surface_v6, events.request_minimize} ptr

    , xdgSurfaceEvtMove = #{ptr struct wlr_xdg_surface_v6, events.request_move} ptr
    , xdgSurfaceEvtResize = #{ptr struct wlr_xdg_surface_v6, events.request_resize} ptr
    , xdgSurfaceEvtMenu = #{ptr struct wlr_xdg_surface_v6, events.request_show_window_menu} ptr
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


getTitle :: Ptr WlrXdgSurface -> IO (Maybe Text)
getTitle ptr = textFromNull =<< #{peek struct wlr_xdg_surface_v6, title} ptr

getAppId :: Ptr WlrXdgSurface -> IO (Maybe Text)
getAppId ptr = textFromNull =<< #{peek struct wlr_xdg_surface_v6, app_id} ptr
