{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Wayland.WlRoots.Surface
    ( WlrSurface
    , surfaceGetTexture

    , createSurface
    , makeSubsurface
    , surfaceGetMain

    , WlrSurfaceState

    , WlrFrameCallback
    , callbackGetResource
    , surfaceGetCallbacks
    , callbackGetCallback

    , getPendingState
    , getCurrentState

    , WlrSubSurface
    , subSurfaceGetSurface
    , surfaceGetSubs
    , subSurfaceGetBox
    , surfaceGetInputRegion

    , getSurfaceResource
    , subSurfaceAt
    , surfaceHasDamage

    , WlrSurfaceEvents (..)
    , getWlrSurfaceEvents

    , surfaceGetScale
    , surfaceGetSize
    , surfaceSendEnter
    , surfaceSendLeave
    , getSurfaceDamage
    , subSurfaceGetDestroyEvent
    , surfaceGetTransform

    , pokeSurfaceData
    , peekSurfaceData

    , surfaceFromResource
    , surfaceHasBuffer
    )
where

#include <wlr/types/wlr_surface.h>

import Data.Composition ((.:))
import Data.Word (Word8, Word32)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, castPtr, plusPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (alloca)

import Graphics.Pixman (PixmanRegion32 (..), pixmanRegionNotEmpty)
import Graphics.Wayland.Signal
import Graphics.Wayland.Server (Callback(..), OutputTransform (..))

import Graphics.Wayland.List (getListFromHead)
import Graphics.Wayland.Resource (WlResource)
import Graphics.Wayland.WlRoots.Box (WlrBox(..), Point (..))
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Render (Texture, Renderer)

data WlrSurface

pokeSurfaceData :: Ptr WlrSurface -> Ptr a -> IO ()
pokeSurfaceData = #{poke struct wlr_surface, data}

peekSurfaceData :: Ptr WlrSurface -> IO (Ptr a)
peekSurfaceData = #{peek struct wlr_surface, data}

foreign import ccall unsafe "wlr_surface_create" c_create :: Ptr WlResource -> Ptr Renderer -> IO (Ptr WlrSurface)

createSurface :: Ptr WlResource -> Ptr Renderer -> IO (Ptr WlrSurface)
createSurface = throwErrnoIfNull "createSurface" .: c_create

getSurfaceResource :: Ptr WlrSurface -> IO (Ptr WlResource)
getSurfaceResource = #{peek struct wlr_surface, resource}

data WlrSurfaceEvents = WlrSurfaceEvents
    { wlrSurfaceEvtCommit  :: Ptr (WlSignal WlrSurface)
    , wlrSurfaceEvtSubSurf :: Ptr (WlSignal WlrSubSurface)
    , wlrSurfaceEvtDestroy :: Ptr (WlSignal WlrSurface)
    }

getWlrSurfaceEvents :: Ptr WlrSurface -> WlrSurfaceEvents
getWlrSurfaceEvents ptr = WlrSurfaceEvents
    { wlrSurfaceEvtDestroy = #{ptr struct wlr_surface, events.destroy} ptr
    , wlrSurfaceEvtSubSurf = #{ptr struct wlr_surface, events.new_subsurface} ptr
    , wlrSurfaceEvtCommit = #{ptr struct wlr_surface, events.commit} ptr
    }

foreign import ccall unsafe "wlr_surface_make_subsurface" c_make_subsurface :: Ptr WlrSurface -> Ptr WlrSurface -> Word32 -> IO ()

makeSubsurface :: Ptr WlrSurface -> Ptr WlrSurface -> Word32 -> IO ()
makeSubsurface = c_make_subsurface

surfaceGetTexture :: Ptr WlrSurface -> IO (Ptr Texture)
surfaceGetTexture = #{peek struct wlr_surface, texture}

foreign import ccall unsafe "wlr_surface_get_main_surface" c_get_main_surface :: Ptr WlrSurface -> IO (Ptr WlrSurface)

surfaceGetMain :: Ptr WlrSurface -> IO (Ptr WlrSurface)
surfaceGetMain = c_get_main_surface


data WlrSurfaceState

stateGetSubsurfaceBox :: Ptr WlrSurfaceState -> IO WlrBox
stateGetSubsurfaceBox state = do
    x :: Word32 <- #{peek struct wlr_surface_state, subsurface_position.x} state
    y :: Word32 <- #{peek struct wlr_surface_state, subsurface_position.y} state

    width :: CInt <- #{peek struct wlr_surface_state, width} state
    height :: CInt <- #{peek struct wlr_surface_state, height} state

    pure WlrBox
        { boxX = fromIntegral x
        , boxY = fromIntegral y
        , boxHeight = fromIntegral height
        , boxWidth = fromIntegral width
        }

stateGetTransform :: Ptr WlrSurfaceState -> IO OutputTransform
stateGetTransform = fmap OutputTransform . #{peek struct wlr_surface_state, transform}

surfaceGetTransform :: Ptr WlrSurface -> IO OutputTransform
surfaceGetTransform surf = stateGetTransform =<< getCurrentState surf

stateGetScale :: Ptr WlrSurfaceState -> IO Word32
stateGetScale = #{peek struct wlr_surface_state, scale}

surfaceGetScale :: Ptr WlrSurface -> IO Word32
surfaceGetScale surf = stateGetScale =<< getCurrentState surf

stateGetInputRegion :: Ptr WlrSurfaceState -> Ptr PixmanRegion32
stateGetInputRegion = #{ptr struct wlr_surface_state, input}

surfaceGetInputRegion :: Ptr WlrSurface -> IO (Ptr PixmanRegion32)
surfaceGetInputRegion = fmap stateGetInputRegion . getCurrentState

stateGetSize :: Ptr WlrSurfaceState -> IO Point
stateGetSize state = do
    width :: CInt <- #{peek struct wlr_surface_state, width} state
    height :: CInt <- #{peek struct wlr_surface_state, height} state

    pure $ Point (fromIntegral width) (fromIntegral height)

surfaceGetSize :: Ptr WlrSurface -> IO Point
surfaceGetSize surf = stateGetSize =<< getCurrentState surf

data WlrFrameCallback

callbackGetResource :: Ptr WlrFrameCallback -> IO (Ptr WlResource)
callbackGetResource = #{peek struct wlr_frame_callback, resource}

surfaceGetCallbacks :: Ptr WlrSurfaceState -> IO [Ptr WlrFrameCallback]
surfaceGetCallbacks ptr =
    let list = #{ptr struct wlr_surface_state, frame_callback_list} ptr
     in getListFromHead list #{offset struct wlr_frame_callback, link}

callbackGetCallback :: Ptr WlrFrameCallback -> IO Callback
callbackGetCallback = fmap (Callback . castPtr) . callbackGetResource


getPendingState :: Ptr WlrSurface -> IO (Ptr WlrSurfaceState)
getPendingState = #{peek struct wlr_surface, pending}

getCurrentState :: Ptr WlrSurface -> IO (Ptr WlrSurfaceState)
getCurrentState = #{peek struct wlr_surface, current}

stateHasDamage :: Ptr WlrSurfaceState -> IO Bool
stateHasDamage ptr = pixmanRegionNotEmpty . PixmanRegion32 $ #{ptr struct wlr_surface_state, surface_damage} ptr

surfaceHasDamage :: Ptr WlrSurface -> IO Bool
surfaceHasDamage surf = stateHasDamage =<< getCurrentState surf

getStateDamage :: Ptr WlrSurfaceState -> Ptr PixmanRegion32
getStateDamage = #{ptr struct wlr_surface_state, surface_damage}

getSurfaceDamage :: Ptr WlrSurface -> IO (Maybe PixmanRegion32)
getSurfaceDamage surf = do
    state <- getCurrentState surf
    hasDamage <- stateHasDamage state
    pure $ if hasDamage
        then Just . PixmanRegion32 $ getStateDamage state
        else Nothing

data WlrSubSurface

subSurfaceGetDestroyEvent :: Ptr WlrSubSurface -> Ptr (WlSignal WlrSubSurface)
subSurfaceGetDestroyEvent = #{ptr struct wlr_subsurface, events.destroy}

subSurfaceGetSurface :: Ptr WlrSubSurface -> IO (Ptr WlrSurface)
subSurfaceGetSurface = #{peek struct wlr_subsurface, surface}

surfaceGetSubs :: Ptr WlrSurface -> IO [Ptr WlrSubSurface]
surfaceGetSubs surf = do
    let list = #{ptr struct wlr_surface, subsurface_list} surf
    getListFromHead list #{offset struct wlr_subsurface, parent_link}

subSurfaceGetBox :: Ptr WlrSubSurface -> IO WlrBox
subSurfaceGetBox surf = stateGetSubsurfaceBox =<< getCurrentState =<< subSurfaceGetSurface surf

--struct wlr_subsurface *wlr_surface_subsurface_at(struct wlr_surface *surface,
--surfacedouble sx, double sy, double *sub_x, double *sub_y);

foreign import ccall "wlr_surface_subsurface_at" c_subsurface_at :: Ptr WlrSurface -> Double -> Double -> Ptr Double -> Ptr Double -> IO (Ptr WlrSubSurface)

subSurfaceAt :: Ptr WlrSurface -> Double -> Double -> IO (Maybe (Ptr WlrSurface, Double, Double))
subSurfaceAt surf x y = alloca $ \xptr -> alloca $ \yptr -> do
    ret <- c_subsurface_at surf x y xptr yptr
    if ret == nullPtr
        then pure Nothing
        else do
            sX <- peek xptr
            sY <- peek yptr
            retSurf <- subSurfaceGetSurface ret
            pure $ Just (retSurf, x - sX, y - sY)

foreign import ccall "wlr_surface_send_enter" c_send_enter :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()

surfaceSendEnter :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()
surfaceSendEnter = c_send_enter

foreign import ccall "wlr_surface_send_leave" c_send_leave :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()

surfaceSendLeave :: Ptr WlrSurface -> Ptr WlrOutput -> IO ()
surfaceSendLeave = c_send_leave

foreign import ccall "wlr_surface_from_resource" c_from_resource :: Ptr WlResource -> IO (Ptr WlrSurface)

surfaceFromResource :: Ptr WlResource -> IO (Ptr WlrSurface)
surfaceFromResource = c_from_resource

foreign import ccall "wlr_surface_has_buffer" c_has_buffer :: Ptr WlrSurface -> IO Word8

surfaceHasBuffer :: Ptr WlrSurface -> IO Bool
surfaceHasBuffer =  fmap (/= 0) . c_has_buffer
