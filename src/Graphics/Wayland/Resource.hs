{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.Resource
    ( WlResource
    , getUserData
    , resourceDestroy
    , resourceGetClient
    )
where

import Foreign.Ptr (Ptr)

import Graphics.Wayland.Server (Client (..))

data WlResource

foreign import ccall unsafe "wl_resource_get_user_data" c_get_user_data :: Ptr WlResource -> Ptr a

getUserData :: Ptr WlResource -> Ptr a
getUserData = c_get_user_data

foreign import ccall unsafe "wl_resource_destroy" c_resource_destroy :: Ptr WlResource -> IO ()

resourceDestroy :: Ptr WlResource -> IO ()
resourceDestroy = c_resource_destroy

foreign import ccall unsafe "wl_resource_get_client" c_get_client :: Ptr WlResource -> IO (Ptr Client)

resourceGetClient :: Ptr WlResource -> IO Client
resourceGetClient = fmap Client . c_get_client
