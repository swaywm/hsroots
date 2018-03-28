module Graphics.Wayland.WlRoots.SurfaceLayers
    ( LayerShell (..)
    , LayerShellEvents (..)
    , SurfaceState (..)
    , getLayerShellEvents
    , layerShellCreate
    , layerShellDestroy

    , configureSurface
    , closeSurface

    , LayerSurfaceEvents
    , getLayerSurfaceEvents

    , getSurfaceState
    )
where

#include <wlr/types/wlr_layer_shell.h>

import Data.Word (Word32)
import Data.Int (Int32)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable

import Graphics.Wayland.Server (DisplayServer (..))

import Graphics.Wayland.Signal (WlSignal)

data LayerShell = LayerShell { unLS :: Ptr LayerShell }
data LayerSurface = LayerSurface { unLSS :: Ptr LayerSurface }

data LayerShellEvents = LayerShellEvents
    { layerShellEventsSurface :: Ptr (WlSignal LayerSurface)
    }

getLayerShellEvents :: LayerShell -> LayerShellEvents
getLayerShellEvents (LayerShell ptr) = LayerShellEvents
    { layerShellEventsSurface = #{ptr struct wlr_layer_shell, events.new_surface} ptr
    }

data SurfaceState = SurfaceState
    { surfaceStateAnchor :: Word32 -- TODO: Make list of enum
    , surfaceStateExclusive :: Int32
    , surfaceStateMarginTop :: Word32
    , surfaceStateMarginBottom :: Word32
    , surfaceStateMarginLeft :: Word32
    , surfaceStateMarginRight :: Word32
    , surfaceStateKeyboard :: Bool
    , surfaceStateDesiredWidth :: Word32
    , surfaceStateDesiredHeight :: Word32
    , surfaceStateActualWidth :: Word32
    , surfaceStateActualHeight :: Word32
    }

foreign import ccall unsafe "wlr_layer_shell_create" c_create :: Ptr DisplayServer -> IO (Ptr LayerShell)

layerShellCreate :: DisplayServer -> IO LayerShell
layerShellCreate (DisplayServer dsp) = LayerShell <$>
    throwErrnoIfNull "layerShellCreate" (c_create dsp)

foreign import ccall "wlr_layer_shell_destroy" c_destroy :: Ptr LayerShell -> IO ()

layerShellDestroy :: LayerShell -> IO ()
layerShellDestroy = c_destroy . unLS

foreign import ccall unsafe "wlr_layer_surface_configure" c_configure :: Ptr LayerSurface -> Word32 -> Word32 -> IO ()

configureSurface :: LayerSurface -> Word32 -> Word32 -> IO ()
configureSurface (LayerSurface ptr) width height = c_configure ptr width height

foreign import ccall unsafe "wlr_layer_surface_close" c_close :: Ptr LayerSurface -> IO ()

closeSurface :: LayerSurface -> IO ()
closeSurface = c_close . unLSS

data LayerSurfaceEvents = LayerSurfaceEvents
    { layerSurfaceEventsDestroy :: Ptr (WlSignal LayerSurface)
    , layerSurfaceEventsMap     :: Ptr (WlSignal LayerSurface)
    , layerSurfaceEventsUnmap   :: Ptr (WlSignal LayerSurface)
    }

getLayerSurfaceEvents :: LayerSurface -> LayerSurfaceEvents
getLayerSurfaceEvents (LayerSurface ptr) = LayerSurfaceEvents
    { layerSurfaceEventsDestroy = #{ptr struct wlr_layer_surface, events.destroy} ptr
    , layerSurfaceEventsMap     = #{ptr struct wlr_layer_surface, events.map} ptr
    , layerSurfaceEventsUnmap   = #{ptr struct wlr_layer_surface, events.unmap} ptr
    }

instance Storable SurfaceState where
    sizeOf _ = #{size struct wlr_layer_surface_state}
    alignment _ = #{alignment struct wlr_layer_surface_state}
    peek ptr = SurfaceState
        <$> #{peek struct wlr_layer_surface_state, anchor} ptr
        <*> #{peek struct wlr_layer_surface_state, exclusive_zone} ptr
        <*> #{peek struct wlr_layer_surface_state, margin.top} ptr
        <*> #{peek struct wlr_layer_surface_state, margin.bottom} ptr
        <*> #{peek struct wlr_layer_surface_state, margin.left} ptr
        <*> #{peek struct wlr_layer_surface_state, margin.right} ptr
        <*> #{peek struct wlr_layer_surface_state, keyboard_interactive} ptr
        <*> #{peek struct wlr_layer_surface_state, desired_width} ptr
        <*> #{peek struct wlr_layer_surface_state, desired_height} ptr
        <*> #{peek struct wlr_layer_surface_state, actual_width} ptr
        <*> #{peek struct wlr_layer_surface_state, actual_height} ptr
    poke = error "No reason to poke LayerShell SurfaceStates for now"

getSurfaceState :: LayerSurface -> IO SurfaceState
getSurfaceState = #{peek struct wlr_layer_surface, current} . unLSS
