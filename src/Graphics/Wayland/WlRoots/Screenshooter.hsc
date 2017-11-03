module Graphics.Wayland.WlRoots.Screenshooter
    ( WlrScreenshooter
    , screenshooterCreate
    , screenshooterDestroy
    )
where

import Foreign.Ptr (Ptr)

import Graphics.Wayland.Server (DisplayServer(..))

import Graphics.Wayland.WlRoots.Render (Renderer)

data WlrScreenshooter

foreign import ccall "wlr_screenshooter_create" c_create :: Ptr DisplayServer -> Ptr Renderer -> IO (Ptr WlrScreenshooter)

screenshooterCreate :: DisplayServer -> Ptr Renderer -> IO (Ptr WlrScreenshooter)
screenshooterCreate (DisplayServer dsp) rend = c_create dsp rend

foreign import ccall "wlr_screenshooter_destroy" c_destroy :: Ptr WlrScreenshooter -> IO ()

screenshooterDestroy :: Ptr WlrScreenshooter -> IO ()
screenshooterDestroy = c_destroy
