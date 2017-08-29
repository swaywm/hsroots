{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Render
    ( Renderer
    , Texture
    , textureCreate
    , renderWithMatrix
    , bufferIsDrm
    , rendererDestroy
    , uploadPixels
    )
where

import Graphics.Wayland.Server (Buffer)
import Foreign.Ptr (Ptr)
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIf_)
import Foreign.C.Types (CFloat(..), CInt(..))
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

--bool wlr_texture_upload_pixels(struct wlr_texture *tex,
--		enum wl_shm_format format, int stride, int width, int height,
--		const unsigned char *pixels);
foreign import ccall unsafe "wlr_texture_upload_pixels" c_upload_pixels :: Ptr Texture -> CInt -> CInt -> CInt -> CInt -> Ptr a -> IO Bool

uploadPixels :: Ptr Texture -> Int -> Int -> Int -> Int -> Ptr a -> IO ()
uploadPixels tex format strice width height pixels = 
    throwErrnoIf_ not "uploadPixels" $ c_upload_pixels tex (fromIntegral format) (fromIntegral strice) (fromIntegral width) (fromIntegral height) pixels
