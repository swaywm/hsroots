{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Render
    ( Renderer
    , Texture
    , textureCreate
    , renderWithMatrix
    , bufferIsDrm
    , rendererDestroy
    )
where

import Graphics.Wayland.Server (Buffer)
import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIf_)
import Foreign.C.Types (CFloat(..))
import Graphics.Wayland.WlRoots.Render.Matrix (Matrix(..))

data Renderer
data Texture


foreign import ccall unsafe "wlr_render_texture_create" c_texture_create :: Ptr Renderer -> IO (Ptr Texture)

textureCreate :: Ptr Renderer -> IO (Ptr Texture)
textureCreate = throwErrnoIfNull "textureCreate" . c_texture_create


foreign import ccall unsafe "wlr_render_with_matrix" c_render_with_matrix :: Ptr Renderer -> Ptr Texture -> Ptr CFloat -> IO Bool

renderWithMatrix :: Ptr Renderer -> Ptr Texture -> Matrix -> IO ()
renderWithMatrix r t (Matrix m) = throwErrnoIf_ not "renderWithMatrix" $ c_render_with_matrix r t m


--foreign import ccall unsafe "wlr_render_colored_quad" c_render_colored_quad :: Ptr Renderer -> Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "wlr_renderer_buffer_is_drm" c_buffer_is_drm :: Ptr Renderer -> Ptr Buffer -> IO Bool

bufferIsDrm :: Ptr Renderer -> Ptr Buffer -> IO Bool
bufferIsDrm = c_buffer_is_drm


foreign import ccall unsafe "wlr_renderer_destroy" c_renderer_destroy :: Ptr Renderer -> IO ()

rendererDestroy :: Ptr Renderer -> IO ()
rendererDestroy = c_renderer_destroy


