module Graphics.Wayland.WlRoots.Screenshooter
    ( WlrScreenshooter
    , screenshooterCreate
    , screenshooterDestroy
    , getScreenshooterGlobal
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_screenshooter.h>

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

import Graphics.Wayland.Server (DisplayServer(..))
import Graphics.Wayland.Global (WlGlobal)

import Graphics.Wayland.WlRoots.Render (Renderer)

data WlrScreenshooter

foreign import ccall "wlr_screenshooter_create" c_create :: Ptr DisplayServer -> Ptr Renderer -> IO (Ptr WlrScreenshooter)

screenshooterCreate :: DisplayServer -> Ptr Renderer -> IO (Ptr WlrScreenshooter)
screenshooterCreate (DisplayServer dsp) rend = c_create dsp rend

foreign import ccall "wlr_screenshooter_destroy" c_destroy :: Ptr WlrScreenshooter -> IO ()

screenshooterDestroy :: Ptr WlrScreenshooter -> IO ()
screenshooterDestroy = c_destroy

getScreenshooterGlobal :: Ptr WlrScreenshooter -> IO (Ptr WlGlobal)
getScreenshooterGlobal = #{peek struct wlr_screenshooter, global}
