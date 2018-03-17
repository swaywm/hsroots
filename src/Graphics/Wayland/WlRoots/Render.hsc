{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Render
    ( Renderer
    , Texture
    , textureCreate

    , renderWithMatrix
    , renderWithMatrixA
    , bufferIsDrm
    , rendererDestroy
    , uploadPixels

    , rendererBegin
    , rendererEnd

    , doRender
    , isTextureValid
    , getTextureSize

    , renderColoredQuad

    , rendererScissor
    , rendererClear
    )
where

#include <wlr/render.h>

import Foreign.Storable (Storable(..))
import Graphics.Wayland.Server (Buffer)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIf_)
import Foreign.C.Types (CFloat(..), CInt(..))
import Graphics.Wayland.WlRoots.Render.Matrix (Matrix(..))
import Graphics.Wayland.WlRoots.Render.Color (Color)
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Foreign.Marshal.Utils (with)
import Control.Exception (bracket_)

import Graphics.Wayland.WlRoots.Box (WlrBox)

data Renderer
data Texture

foreign import ccall unsafe "wlr_renderer_begin" c_renderer_begin :: Ptr Renderer -> Ptr WlrOutput -> IO ()

rendererBegin :: Ptr Renderer -> Ptr WlrOutput -> IO ()
rendererBegin = c_renderer_begin


foreign import ccall unsafe "wlr_renderer_end" c_renderer_end :: Ptr Renderer -> IO ()

rendererEnd :: Ptr Renderer -> IO ()
rendererEnd = c_renderer_end


doRender :: Ptr Renderer -> Ptr WlrOutput -> IO a -> IO a
doRender renderer output = bracket_
    (rendererBegin renderer output)
    (rendererEnd renderer)


foreign import ccall unsafe "wlr_render_texture_create" c_texture_create :: Ptr Renderer -> IO (Ptr Texture)

textureCreate :: Ptr Renderer -> IO (Ptr Texture)
textureCreate = throwErrnoIfNull "textureCreate" . c_texture_create


foreign import ccall unsafe "wlr_render_texture_with_matrix" c_render_with_matrix :: Ptr Renderer -> Ptr Texture -> Ptr CFloat -> CFloat -> IO Bool

renderWithMatrix :: Ptr Renderer -> Ptr Texture -> Matrix -> IO ()
renderWithMatrix r t (Matrix m) = throwErrnoIf_ not "renderWithMatrix" $ c_render_with_matrix r t m 1.0

renderWithMatrixA :: Ptr Renderer -> Ptr Texture -> Matrix -> CFloat -> IO ()
renderWithMatrixA r t (Matrix m) a = throwErrnoIf_ not "renderWithMatrixA" $ c_render_with_matrix r t m a

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
    pure (ret /= 0)


foreign import ccall unsafe "wlr_render_colored_quad" c_colored_quad :: Ptr Renderer -> Ptr Color -> Ptr CFloat -> IO ()

renderColoredQuad :: Ptr Renderer -> Color -> Matrix -> IO ()
renderColoredQuad rend col (Matrix m) = with col $ \cptr ->
    c_colored_quad rend cptr m

foreign import ccall unsafe "wlr_renderer_clear" c_clear :: Ptr Renderer -> Ptr Color -> IO ()

rendererClear :: Ptr Renderer -> Color -> IO ()
rendererClear rend col = with col $ c_clear rend

getTextureSize :: Ptr Texture -> IO (Int, Int)
getTextureSize ptr = do
    width :: CInt <- #{peek struct wlr_texture, width} ptr
    height :: CInt <- #{peek struct wlr_texture, height} ptr
    pure (fromIntegral width, fromIntegral height)

foreign import ccall unsafe "wlr_renderer_scissor" c_scissor :: Ptr Renderer -> Ptr WlrBox -> IO ()

rendererScissor :: Ptr Renderer -> Maybe WlrBox -> IO ()
rendererScissor rend (Just box) = with box $ c_scissor rend
rendererScissor rend Nothing =  c_scissor rend nullPtr
