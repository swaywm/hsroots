{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Render
    ( Renderer
    , Texture
    , textureCreate

    , getMatrix
    , renderWithMatrix
    , bufferIsDrm
    , rendererDestroy
    , uploadPixels

    , rendererBegin
    , rendererEnd

    , doRender
    , isTextureValid
    )
where

#include <wlr/render.h>

import System.IO

import Foreign.Storable (Storable(..))
import Graphics.Wayland.Server (Buffer)
import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIf_)
import Foreign.C.Types (CFloat(..), CInt(..))
import Graphics.Wayland.WlRoots.Render.Matrix (Matrix(..))
import Graphics.Wayland.WlRoots.Output (Output)
import Control.Exception (bracket_)

data Renderer
data Texture

foreign import ccall unsafe "wlr_renderer_begin" c_renderer_begin :: Ptr Renderer -> Ptr Output -> IO ()

rendererBegin :: Ptr Renderer -> Ptr Output -> IO ()
rendererBegin = c_renderer_begin


foreign import ccall unsafe "wlr_renderer_end" c_renderer_end :: Ptr Renderer -> IO ()

rendererEnd :: Ptr Renderer -> IO ()
rendererEnd = c_renderer_end


doRender :: Ptr Renderer -> Ptr Output -> IO a -> IO a
doRender renderer output = bracket_
    (rendererBegin renderer output)
    (rendererEnd renderer)


foreign import ccall unsafe "wlr_render_texture_create" c_texture_create :: Ptr Renderer -> IO (Ptr Texture)

textureCreate :: Ptr Renderer -> IO (Ptr Texture)
textureCreate = throwErrnoIfNull "textureCreate" . c_texture_create


foreign import ccall unsafe "wlr_texture_get_matrix" c_get_matrix :: Ptr Texture -> Matrix -> Matrix -> CInt -> CInt -> IO ()

getMatrix :: Ptr Texture -> Matrix -> Matrix -> Int -> Int -> IO ()
getMatrix tex matrix projection x y =
    c_get_matrix tex matrix projection (fromIntegral x) (fromIntegral y)


foreign import ccall unsafe "wlr_render_with_matrix" c_render_with_matrix :: Ptr Renderer -> Ptr Texture -> Ptr CFloat -> IO Bool

renderWithMatrix :: Ptr Renderer -> Ptr Texture -> Matrix -> IO ()
renderWithMatrix r t (Matrix m) = throwErrnoIf_ not "renderWithMatrix" $ c_render_with_matrix r t m


foreign import ccall unsafe "wlr_renderer_buffer_is_drm" c_buffer_is_drm :: Ptr Renderer -> Ptr Buffer -> IO Bool

bufferIsDrm :: Ptr Renderer -> Ptr Buffer -> IO Bool
bufferIsDrm = c_buffer_is_drm


foreign import ccall unsafe "wlr_renderer_destroy" c_renderer_destroy :: Ptr Renderer -> IO ()

rendererDestroy :: Ptr Renderer -> IO ()
rendererDestroy = c_renderer_destroy


foreign import ccall unsafe "wlr_texture_upload_pixels" c_upload_pixels :: Ptr Texture -> CInt -> CInt -> CInt -> CInt -> Ptr a -> IO Bool

uploadPixels :: Ptr Texture -> Int -> Int -> Int -> Int -> Ptr a -> IO ()
uploadPixels tex format strice width height pixels = 
    throwErrnoIf_ not "uploadPixels" $ c_upload_pixels tex (fromIntegral format) (fromIntegral strice) (fromIntegral width) (fromIntegral height) pixels


isTextureValid :: Ptr Texture -> IO Bool
isTextureValid tex = do
    ret :: CInt <- #{peek struct wlr_texture, valid} tex
    hPutStr stderr "Valid: "
    hPutStrLn stderr  $ show ret
    pure (ret /= 0)
