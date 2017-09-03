{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.Resource
    ( WlResource
    , getUserData
    , resourceDestroy
    )
where

import Foreign.Ptr (Ptr)

data WlResource

foreign import ccall unsafe "wl_resource_get_user_data" c_get_user_data :: Ptr WlResource -> Ptr a

getUserData :: Ptr WlResource -> Ptr a
getUserData = c_get_user_data

foreign import ccall unsafe "wl_resource_destroy" c_resource_destroy :: Ptr WlResource -> IO ()

resourceDestroy :: Ptr WlResource -> IO ()
resourceDestroy = c_resource_destroy
