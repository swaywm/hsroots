module Graphics.Wayland.WlRoots.GammaControl
    ( WlrGammaManager
    , createGammaManager
    , destroyGammaManager
    , getGammaGlobal
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_gamma_control.h>

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

import Graphics.Wayland.Server (DisplayServer (..))
import Graphics.Wayland.Global (WlGlobal)

data WlrGammaManager

foreign import ccall "wlr_gamma_control_manager_create" c_create :: Ptr DisplayServer -> IO (Ptr WlrGammaManager)

createGammaManager :: DisplayServer -> IO (Ptr WlrGammaManager)
createGammaManager (DisplayServer ptr) = c_create ptr

foreign import ccall "wlr_gamma_control_manager_destroy" c_destroy :: Ptr WlrGammaManager -> IO ()

destroyGammaManager :: Ptr WlrGammaManager -> IO ()
destroyGammaManager = c_destroy

getGammaGlobal :: Ptr WlrGammaManager -> IO (Ptr WlGlobal)
getGammaGlobal = #{peek struct wlr_gamma_control_manager, global}
