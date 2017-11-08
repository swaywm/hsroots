{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.XWayland
    ( XWayland
    , xwaylandCreate
    , xwayBindNew

    , X11Surface
    , xwaySurfaceGetSurface

    , xwayCloseSurface
    , getX11SurfaceDataPtr

    , WlrX11SurfaceEvents (..)
    , getX11SurfaceEvents
    , activateX11Surface
    , configureX11Surface
    , getX11SurfacePosition
    , setX11SurfacePosition
    , getX11SurfaceGeometry

    , x11SurfaceOverrideRedirect
    )
where

#include <wlr/xwayland.h>


import System.IO
import Data.Word (Word16, Word32)
import Data.Int (Int16)
import Data.Word (Word8)
import Foreign.Storable (Storable(..))
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.StablePtr
    ( newStablePtr
    , castStablePtrToPtr
    )
import Graphics.Wayland.Signal
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.List (getListFromHead)
import Graphics.Wayland.Resource (getUserData)
import Graphics.Wayland.Server (DisplayServer (..))
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Graphics.Wayland.WlRoots.Box (Point(..), WlrBox(..))

data XWayland

foreign import ccall unsafe "wlr_xwayland_create" c_xwayland_create :: Ptr DisplayServer -> Ptr WlrCompositor -> IO (Ptr XWayland)

xwaylandCreate :: DisplayServer -> Ptr WlrCompositor -> IO (Ptr XWayland)
xwaylandCreate (DisplayServer ptr) comp =
    throwErrnoIfNull "xwaylandCreate" $ c_xwayland_create ptr comp

xwayBindNew :: Ptr XWayland -> (Ptr X11Surface -> IO ()) -> IO ()
xwayBindNew shell handler = do
    let signal = #{ptr struct wlr_xwayland, events.new_surface} shell
    handler <- addListener (WlListener handler) signal
    sptr <- newStablePtr handler
    poke (#{ptr struct wlr_xwayland, data} shell) (castStablePtrToPtr sptr)


data X11Surface

xwaySurfaceGetSurface :: Ptr X11Surface -> IO (Ptr WlrSurface)
xwaySurfaceGetSurface = #{peek struct wlr_xwayland_surface, surface}

foreign import ccall "wlr_xwayland_surface_close" c_close :: Ptr XWayland -> Ptr X11Surface -> IO ()

xwayCloseSurface :: Ptr XWayland -> Ptr X11Surface -> IO ()
xwayCloseSurface = c_close

getX11SurfaceDataPtr :: Ptr X11Surface -> Ptr (Ptr a)
getX11SurfaceDataPtr = #{ptr struct wlr_xwayland_surface, data}

data WlrX11SurfaceEvents = WlrX11SurfaceEvents
    { x11SurfacEvtDestroy :: Ptr (WlSignal X11Surface)

    }


getX11SurfaceEvents :: Ptr X11Surface -> WlrX11SurfaceEvents
getX11SurfaceEvents ptr = WlrX11SurfaceEvents
    { x11SurfacEvtDestroy = #{ptr struct wlr_xwayland_surface, events.destroy} ptr

    }

foreign import ccall "wlr_xwayland_surface_activate" c_activate :: Ptr XWayland -> Ptr X11Surface -> IO ()

activateX11Surface :: Ptr XWayland -> Maybe (Ptr X11Surface) -> IO ()
activateX11Surface xway Nothing = activateX11Surface xway (Just nullPtr)
activateX11Surface xway (Just ptr) = c_activate xway ptr

foreign import ccall "wlr_xwayland_surface_configure" c_configure :: Ptr XWayland -> Ptr X11Surface -> Int16 -> Int16 -> Word32 -> Word32 -> IO ()

configureX11Surface :: Ptr XWayland -> Ptr X11Surface -> Int16 -> Int16 -> Word32 -> Word32 -> IO ()
configureX11Surface xway surf x y width height =
    c_configure xway surf x y width height


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
