{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.WlRoots.Backend
    ( Backend
    , backendAutocreate
    , backendStart
    , backendDestroy
    , backendGetEgl

    , BackendSignals (..)
    , backendGetSignals
    )
where

#include <wlr/backend.h>

import Foreign.Ptr (Ptr, plusPtr)
import Graphics.Wayland.Server (DisplayServer(..))
import Foreign.C.Error (throwErrnoIfNull, throwErrnoIf_)
import Graphics.Wayland.WlRoots.Egl (EGL)
import Graphics.Wayland.WlRoots.Output (WlrOutput)
import Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.Signal (WlSignal)

data Backend

foreign import ccall unsafe "wlr_backend_autocreate" c_backend_autocreate :: Ptr DisplayServer -> IO (Ptr Backend)

backendAutocreate :: DisplayServer -> IO (Ptr Backend)
backendAutocreate (DisplayServer ptr) = throwErrnoIfNull "backendAutocreate" $ c_backend_autocreate ptr


foreign import ccall safe "wlr_backend_start" c_backend_start :: Ptr Backend -> IO Bool

backendStart :: Ptr Backend -> IO ()
backendStart = throwErrnoIf_ not "backendStart" . c_backend_start


foreign import ccall safe "wlr_backend_destroy" c_backend_destroy :: Ptr Backend -> IO ()

backendDestroy :: Ptr Backend -> IO ()
backendDestroy = c_backend_destroy


foreign import ccall unsafe "wlr_backend_get_egl" c_backend_get_egl :: Ptr Backend -> IO (Ptr EGL)

backendGetEgl :: Ptr Backend -> IO (Ptr EGL)
backendGetEgl = throwErrnoIfNull "backendGetEgl" . c_backend_get_egl

data BackendSignals = BackendSignals
    { inputAdd     :: Ptr (WlSignal InputDevice)
    , inputRemove  :: Ptr (WlSignal InputDevice)
    , outputAdd    :: Ptr (WlSignal WlrOutput)
    , outputRemove :: Ptr (WlSignal WlrOutput)
    }

backendGetSignals :: Ptr Backend -> BackendSignals
backendGetSignals ptr = 
    let input_add = #{ptr struct wlr_backend, events.input_add} ptr
        input_remove = #{ptr struct wlr_backend, events.input_remove} ptr
        output_add = #{ptr struct wlr_backend, events.output_add} ptr
        output_remove = #{ptr struct wlr_backend, events.output_remove} ptr
     in BackendSignals
         { inputAdd = input_add
         , inputRemove = input_remove
         , outputAdd = output_add
         , outputRemove = output_remove
         }
