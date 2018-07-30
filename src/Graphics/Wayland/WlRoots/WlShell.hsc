{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.WlShell
    ( WlrWlShell (..)
    , shellCreate
    , shellDestroy

    , WlrWlShellSurface (..)
    , setWlShellListener


    , configureWlShellSurface
    , shellSurfaceAt
    , wlShellSurfaceGetSurface

    , getTitle
    , getClass

    , WlrWlSurfaceEvents (..)
    , getWlrWlSurfaceEvents
    , getWlShellPopups
    , isPopup
    , getTransientPosition

    , getClient
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_wl_shell.h>

import Data.Composition ((.:))
import Data.Int (Int32)
import Data.Text (Text)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (Storable(..))

import Graphics.Wayland.List (getListFromHead)
import Graphics.Wayland.Server (DisplayServer(..), Client (..))
import Graphics.Wayland.Signal (WlSignal, addListener, WlListener (..), ListenerToken)
import Graphics.Wayland.WlRoots.Surface (WlrSurface)
import Utility (textFromNull)


newtype WlrWlShell = WlrWlShell { unWlr :: Ptr WlrWlShell }

foreign import ccall unsafe "wlr_wl_shell_create" c_shell_create :: Ptr DisplayServer -> IO (Ptr WlrWlShell)

shellCreate :: DisplayServer -> IO WlrWlShell
shellCreate (DisplayServer ptr) =
    WlrWlShell <$> (throwErrnoIfNull "shellCreate" $ c_shell_create ptr)

foreign import ccall "wlr_wl_shell_destroy" c_shell_destroy :: Ptr WlrWlShell -> IO ()

shellDestroy :: WlrWlShell -> IO ()
shellDestroy = c_shell_destroy . unWlr


newtype WlrWlShellSurface = WlrWlShellSurface { unWlrSurf :: Ptr WlrWlShellSurface }

setWlShellListener :: WlrWlShell -> (WlrWlShellSurface -> IO ()) -> IO ListenerToken
setWlShellListener (WlrWlShell ptr) fun =
    let signal = #{ptr struct wlr_wl_shell, events.new_surface} ptr
     in addListener (WlListener $ fun . WlrWlShellSurface) signal


foreign import ccall unsafe "wlr_wl_shell_surface_configure" c_surface_configure :: Ptr WlrWlShellSurface -> CInt -> Int32 -> Int32 -> IO ()

configureWlShellSurface :: WlrWlShellSurface -> Int32 -> Int32 -> IO ()
configureWlShellSurface (WlrWlShellSurface ptr) x y = c_surface_configure ptr 0 x y

foreign import ccall unsafe "wlr_wl_shell_surface_surface_at" c_surface_at :: Ptr WlrWlShellSurface -> Double -> Double -> Ptr Double -> Ptr Double -> IO (Ptr WlrSurface)

shellSurfaceAt :: WlrWlShellSurface -> Double -> Double -> IO (Maybe (Ptr WlrSurface, Double, Double))
shellSurfaceAt (WlrWlShellSurface ptr) x y = alloca $ \xptr -> alloca $ \yptr -> do
    ret <- c_surface_at ptr x y xptr yptr
    if ret == nullPtr
        then pure Nothing
        else Just .: (ret, ,) <$> peek xptr <*> peek yptr

wlShellSurfaceGetSurface :: WlrWlShellSurface -> IO (Maybe (Ptr WlrSurface))
wlShellSurfaceGetSurface (WlrWlShellSurface ptr) = do
    ret <- #{peek struct wlr_wl_shell_surface, surface} ptr
    pure $ if ret == nullPtr
        then Nothing
        else Just ret

getTitle :: WlrWlShellSurface -> IO (Maybe Text)
getTitle (WlrWlShellSurface ptr) = textFromNull =<< #{peek struct wlr_wl_shell_surface, title} ptr

getClass :: WlrWlShellSurface -> IO (Maybe Text)
getClass (WlrWlShellSurface ptr) = textFromNull =<< #{peek struct wlr_wl_shell_surface, class} ptr


data WlrWlSurfaceEvents = WlrWlSurfaceEvents
    { wlrWlSurfaceEvtDestroy :: Ptr (WlSignal WlrWlShellSurface)
    , wlrWlSurfaceEvtPopup   :: Ptr (WlSignal WlrWlShellSurface)
    }

getWlrWlSurfaceEvents :: WlrWlShellSurface -> WlrWlSurfaceEvents
getWlrWlSurfaceEvents (WlrWlShellSurface ptr) = WlrWlSurfaceEvents
    { wlrWlSurfaceEvtDestroy = #{ptr struct wlr_wl_shell_surface, events.destroy} ptr
    , wlrWlSurfaceEvtPopup   = #{ptr struct wlr_wl_shell_surface, events.new_popup} ptr
    }

getWlShellPopups :: WlrWlShellSurface -> IO [WlrWlShellSurface]
getWlShellPopups (WlrWlShellSurface ptr) = 
    let list = #{ptr struct wlr_wl_shell_surface, popups} ptr
     in fmap WlrWlShellSurface <$> getListFromHead list #{offset struct wlr_wl_shell_surface, popup_link}

isPopup :: WlrWlShellSurface -> IO Bool
isPopup (WlrWlShellSurface ptr) = do
    val :: CInt <- #{peek struct wlr_wl_shell_surface, state} ptr
    pure $ val == #{const WLR_WL_SHELL_SURFACE_STATE_POPUP}


getTransientPosition :: WlrWlShellSurface -> IO (Maybe (Int32, Int32))
getTransientPosition (WlrWlShellSurface ptr) = do
    state <- #{peek struct wlr_wl_shell_surface, transient_state} ptr
    if ptr == nullPtr
        then pure Nothing
        else Just <$> do
            x <- #{peek struct wlr_wl_shell_surface_transient_state, x} state
            y <- #{peek struct wlr_wl_shell_surface_transient_state, y} state
            pure (x, y)


getClient :: WlrWlShellSurface -> IO Client
getClient (WlrWlShellSurface ptr) = Client <$> #{peek struct wlr_wl_shell_surface, client} ptr
