module Graphics.Wayland.WlRoots.Backend.Multi
    ( getSession
    , isMulti

    , getSession'
    , addBackend
    )
where

import Data.Word (Word8)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Ptr (Ptr)

import Graphics.Wayland.WlRoots.Backend.Session (WlrSession)
import Graphics.Wayland.WlRoots.Backend (Backend)


foreign import ccall unsafe "wlr_backend_is_multi" c_is_multi :: Ptr Backend -> IO Word8

isMulti :: Ptr Backend -> IO Bool
isMulti = fmap (/= 0) . c_is_multi


foreign import ccall unsafe "wlr_multi_get_session" c_get_session :: Ptr Backend -> IO (Ptr WlrSession)

getSession :: Ptr Backend -> IO (Ptr WlrSession)
getSession = throwErrnoIfNull "getSession" . c_get_session


getSession' :: Ptr Backend -> IO (Maybe (Ptr WlrSession))
getSession' backend = do
    multi <- isMulti backend
    if multi
       then Just <$> getSession backend
       else pure Nothing

foreign import ccall "wlr_multi_backend_add" c_multi_add :: Ptr Backend -> Ptr Backend -> IO ()

addBackend :: Ptr Backend -> Ptr Backend -> IO ()
addBackend = c_multi_add
