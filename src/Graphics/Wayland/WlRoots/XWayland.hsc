{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.XWayland
    ( XWayland
    , xwaylandCreate
    , xwaylandDestroy
    , xwayBindNew
    , xwayReadEvent

    , X11Surface
    , xwaySurfaceGetSurface

    , xwayCloseSurface
    , getX11SurfaceDataPtr

    , ConfigureEvent (..)
    , MoveEvent (..)
    , ResizeEvent (..)
    , WlrX11SurfaceEvents (..)
    , getX11SurfaceEvents
    , activateX11Surface
    , configureX11Surface
    , getX11SurfacePosition
    , setX11SurfacePosition
    , getX11SurfaceGeometry

    , x11SurfaceOverrideRedirect
    , getTitle
    , getClass
    )
where

#include <wlr/xwayland.h>


import Data.Int (Int16)
import Data.Text (Text)
import Data.Word (Word16, Word32)
import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.StablePtr (newStablePtr , castStablePtrToPtr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.Server (DisplayServer (..))
import Graphics.Wayland.Signal
import Graphics.Wayland.WlRoots.Box (Point(..), WlrBox(..))
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Utility (textFromNull)

data XWayland

foreign import ccall unsafe "wlr_xwayland_create" c_xwayland_create :: Ptr DisplayServer -> Ptr WlrCompositor -> IO (Ptr XWayland)

xwaylandCreate :: DisplayServer -> Ptr WlrCompositor -> IO (Ptr XWayland)
xwaylandCreate (DisplayServer ptr) comp =
    throwErrnoIfNull "xwaylandCreate" $ c_xwayland_create ptr comp

foreign import ccall unsafe "wlr_xwayland_destroy" c_xwayland_destroy :: Ptr XWayland -> IO ()

xwaylandDestroy :: Ptr XWayland -> IO ()
xwaylandDestroy = c_xwayland_destroy

xwayBindNew :: Ptr XWayland -> (Ptr X11Surface -> IO ()) -> IO ()
xwayBindNew shell handler = do
    let signal = #{ptr struct wlr_xwayland, events.new_surface} shell
    tok <- addListener (WlListener handler) signal
    sptr <- newStablePtr tok
    poke (#{ptr struct wlr_xwayland, data} shell) (castStablePtrToPtr sptr)

xwayReadEvent :: Ptr XWayland -> Ptr (WlSignal XWayland)
xwayReadEvent = #{ptr struct wlr_xwayland, events.ready}

data X11Surface

xwaySurfaceGetSurface :: Ptr X11Surface -> IO (Maybe (Ptr WlrSurface))
xwaySurfaceGetSurface ptr = do
    ret <- #{peek struct wlr_xwayland_surface, surface} ptr
    pure $ if ret == nullPtr
        then Nothing
        else Just ret

foreign import ccall "wlr_xwayland_surface_close" c_close :: Ptr X11Surface -> IO ()

xwayCloseSurface :: Ptr X11Surface -> IO ()
xwayCloseSurface = c_close

getX11SurfaceDataPtr :: Ptr X11Surface -> Ptr (Ptr a)
getX11SurfaceDataPtr = #{ptr struct wlr_xwayland_surface, data}

data ConfigureEvent = ConfigureEvent
    { configureEvtSurface :: Ptr X11Surface
    , configureEvtX       :: Int16
    , configureEvtY       :: Int16
    , configureEvtWidth   :: Word16
    , configureEvtHeight  :: Word16
    }

instance Storable ConfigureEvent where
    sizeOf _ = #{size struct wlr_xwayland_surface_configure_event}
    alignment _ = #{alignment struct wlr_xwayland_surface_configure_event}
    peek ptr = ConfigureEvent
        <$> #{peek struct wlr_xwayland_surface_configure_event, surface} ptr
        <*> #{peek struct wlr_xwayland_surface_configure_event, x} ptr
        <*> #{peek struct wlr_xwayland_surface_configure_event, y} ptr
        <*> #{peek struct wlr_xwayland_surface_configure_event, width} ptr
        <*> #{peek struct wlr_xwayland_surface_configure_event, height} ptr
    poke ptr evt = do
        #{poke struct wlr_xwayland_surface_configure_event, surface} ptr $ configureEvtSurface evt
        #{poke struct wlr_xwayland_surface_configure_event, x} ptr $ configureEvtX evt
        #{poke struct wlr_xwayland_surface_configure_event, y} ptr $ configureEvtY evt
        #{poke struct wlr_xwayland_surface_configure_event, width} ptr $ configureEvtWidth evt
        #{poke struct wlr_xwayland_surface_configure_event, height} ptr $ configureEvtHeight evt

data MoveEvent = MoveEvent
    { moveEvtSurface :: Ptr X11Surface
    }

instance Storable MoveEvent where
    sizeOf _ = #{size struct wlr_xwayland_move_event}
    alignment _ = #{alignment struct wlr_xwayland_move_event}
    peek ptr = MoveEvent
        <$> #{peek struct wlr_xwayland_move_event, surface} ptr
    poke ptr evt = do
        #{poke struct wlr_xwayland_move_event, surface} ptr $ moveEvtSurface evt

data ResizeEvent = ResizeEvent
    { resizeEvtSurface :: Ptr X11Surface
    , resizeEvtEdges :: Word32
    }

instance Storable ResizeEvent where
    sizeOf _ = #{size struct wlr_xwayland_resize_event}
    alignment _ = #{alignment struct wlr_xwayland_resize_event}
    peek ptr = ResizeEvent
        <$> #{peek struct wlr_xwayland_resize_event, surface} ptr
        <*> #{peek struct wlr_xwayland_resize_event, edges} ptr
    poke ptr evt = do
        #{poke struct wlr_xwayland_resize_event, surface} ptr $ resizeEvtSurface evt
        #{poke struct wlr_xwayland_resize_event, edges} ptr $ resizeEvtEdges evt

data WlrX11SurfaceEvents = WlrX11SurfaceEvents
    { x11SurfaceEvtDestroy :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtConfigure :: Ptr (WlSignal ConfigureEvent)
    , x11SurfaceEvtMove :: Ptr (WlSignal MoveEvent)
    , x11SurfaceEvtResize :: Ptr (WlSignal ResizeEvent)
    , x11SurfaceEvtFullscreen :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtMaximize :: Ptr (WlSignal X11Surface)

    , x11SurfaceEvtMap :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtUnmap :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtTitle :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtClass :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtParent :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtPid :: Ptr (WlSignal X11Surface)
    , x11SurfaceEvtType :: Ptr (WlSignal X11Surface)
    }


getX11SurfaceEvents :: Ptr X11Surface -> WlrX11SurfaceEvents
getX11SurfaceEvents ptr = WlrX11SurfaceEvents
    { x11SurfaceEvtDestroy = #{ptr struct wlr_xwayland_surface, events.destroy} ptr
    , x11SurfaceEvtConfigure = #{ptr struct wlr_xwayland_surface, events.request_configure} ptr
    , x11SurfaceEvtMove = #{ptr struct wlr_xwayland_surface, events.request_move} ptr
    , x11SurfaceEvtResize = #{ptr struct wlr_xwayland_surface, events.request_resize} ptr
    , x11SurfaceEvtFullscreen = #{ptr struct wlr_xwayland_surface, events.request_fullscreen} ptr
    , x11SurfaceEvtMaximize = #{ptr struct wlr_xwayland_surface, events.request_maximize} ptr

    , x11SurfaceEvtMap = #{ptr struct wlr_xwayland_surface, events.map_notify} ptr
    , x11SurfaceEvtUnmap = #{ptr struct wlr_xwayland_surface, events.unmap_notify} ptr
    , x11SurfaceEvtTitle = #{ptr struct wlr_xwayland_surface, events.set_title} ptr
    , x11SurfaceEvtClass = #{ptr struct wlr_xwayland_surface, events.set_class} ptr
    , x11SurfaceEvtParent = #{ptr struct wlr_xwayland_surface, events.set_parent} ptr
    , x11SurfaceEvtPid = #{ptr struct wlr_xwayland_surface, events.set_pid} ptr
    , x11SurfaceEvtType = #{ptr struct wlr_xwayland_surface, events.set_window_type} ptr
    }

foreign import ccall "wlr_xwayland_surface_activate" c_activate :: Ptr X11Surface -> Bool -> IO ()

activateX11Surface :: Ptr X11Surface -> Bool -> IO ()
activateX11Surface = c_activate

foreign import ccall "wlr_xwayland_surface_configure" c_configure :: Ptr X11Surface -> Int16 -> Int16 -> Word32 -> Word32 -> IO ()

configureX11Surface :: Ptr X11Surface -> Int16 -> Int16 -> Word32 -> Word32 -> IO ()
configureX11Surface surf x y width height =
    c_configure surf x y width height


getX11SurfacePosition :: Ptr X11Surface -> IO (Point)
getX11SurfacePosition surf = do
    x :: Int16 <- #{peek struct wlr_xwayland_surface, x} surf
    y :: Int16 <- #{peek struct wlr_xwayland_surface, y} surf
    pure $ Point (fromIntegral x) (fromIntegral y)


setX11SurfacePosition :: Ptr X11Surface -> Point -> IO ()
setX11SurfacePosition surf (Point x y)= do
    #{poke struct wlr_xwayland_surface, x} surf (fromIntegral x :: Word16)
    #{poke struct wlr_xwayland_surface, y} surf (fromIntegral y :: Word16)

getX11SurfaceGeometry :: Ptr X11Surface -> IO WlrBox
getX11SurfaceGeometry surf = do
    (Point x y) <- getX11SurfacePosition surf
    width :: Word16 <- #{peek struct wlr_xwayland_surface, width} surf
    height :: Word16 <- #{peek struct wlr_xwayland_surface, height} surf
    pure $ WlrBox x y (fromIntegral width) (fromIntegral height)


x11SurfaceOverrideRedirect :: Ptr X11Surface -> IO Bool
x11SurfaceOverrideRedirect ptr = do
    val :: Word8 <- #{peek struct wlr_xwayland_surface, override_redirect} ptr
    pure $ val /= 0

getTitle :: Ptr X11Surface -> IO (Maybe Text)
getTitle ptr = textFromNull =<< #{peek struct wlr_xwayland_surface, title} ptr

getClass :: Ptr X11Surface -> IO (Maybe Text)
getClass ptr = textFromNull =<< #{peek struct wlr_xwayland_surface, class} ptr
