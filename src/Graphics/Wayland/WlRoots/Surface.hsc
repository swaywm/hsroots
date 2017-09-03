{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Surface
    ( WlrSurface
    , withSurfaceMatrix
    , flushDamage
    , surfaceGetTexture

    , WlrFrameCallback
    , callbackGetResource
    , surfaceGetCallbacks
    , callbackGetCallback
    )
where

#include <wlr/types/wlr_surface.h>


import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.C.Types (CFloat(..))

import Graphics.Wayland.Resource (WlResource)
import Graphics.Wayland.WlRoots.Render.Matrix (Matrix(..), withMatrix)
import Graphics.Wayland.WlRoots.Render (Texture)
import Graphics.Wayland.List (getListFromHead)
import Graphics.Wayland.Server (Callback(..))

data WlrSurface

foreign import ccall unsafe "wlr_surface_flush_damage" c_flush_damage :: Ptr WlrSurface -> IO ()

flushDamage :: Ptr WlrSurface -> IO ()
flushDamage = c_flush_damage


foreign import ccall unsafe "wlr_surface_get_matrix" c_get_matrix :: Ptr WlrSurface -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

getSurfaceMatrix :: Ptr WlrSurface -> Matrix -> Matrix -> Matrix -> IO ()
getSurfaceMatrix ptr (Matrix mat) (Matrix projection) (Matrix transform) =
    c_get_matrix ptr mat projection transform

withSurfaceMatrix :: Ptr WlrSurface -> Matrix -> Matrix -> (Matrix -> IO a) -> IO a
withSurfaceMatrix ptr proj trans act = withMatrix $ \mat -> do
    getSurfaceMatrix ptr mat proj trans
    act mat

surfaceGetTexture :: Ptr WlrSurface -> IO (Ptr Texture)
surfaceGetTexture = #{peek struct wlr_surface, texture}


data WlrFrameCallback

callbackGetResource :: Ptr WlrFrameCallback -> IO (Ptr WlResource)
callbackGetResource = #{peek struct wlr_frame_callback, resource}

surfaceGetCallbacks :: Ptr WlrSurface -> IO [Ptr WlrFrameCallback]
surfaceGetCallbacks ptr =
    let list = #{ptr struct wlr_surface, frame_callback_list} ptr
     in getListFromHead list #{offset struct wlr_frame_callback, link}

callbackGetCallback :: Ptr WlrFrameCallback -> IO Callback
callbackGetCallback = fmap (Callback . castPtr) . callbackGetResource
