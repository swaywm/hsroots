{-# LANGUAGE EmptyDataDecls #-}
module Cat
    ( getCatTexture
    )
where

#include "cat.h"
#include <wayland-server.h>

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..))
import Graphics.Wayland.WlRoots.Render (Texture, Renderer, textureCreate, uploadPixels)

data CatImage

catWidth :: Ptr CatImage -> IO CInt
catWidth = #{peek struct gimp_texture, width}

catHeight :: Ptr CatImage -> IO CInt
catHeight = #{peek struct gimp_texture, height}

catData :: Ptr CatImage -> Ptr ()
catData = #{ptr struct gimp_texture, pixel_data}


foreign import ccall unsafe "cat_image" cat_image :: Ptr CatImage

getCatTexture :: Ptr Renderer -> IO (Ptr Texture)
getCatTexture renderer = do
    texture <- textureCreate renderer
    let image = cat_image
    width <- fromIntegral <$> catWidth image
    height <- fromIntegral <$> catHeight image
    uploadPixels texture #{const WL_SHM_FORMAT_ABGR8888} width width height (catData image)
    pure texture
