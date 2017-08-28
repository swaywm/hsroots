{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Wayland.Signal
    ( WlSignal
    , WlListener(..)
    , ListenerToken

    , makeListenerPtr
    , addListener
    , removeListener
    , removeListener'
    )
where

-- We need the wl_lisener in scope
#include <wayland-server.h>

import Foreign.Storable (Storable(..))
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, FunPtr, plusPtr, freeHaskellFunPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)

data WlSignal a
data WlList

newtype WlListener a = WlListener (Ptr a -> IO ())
data ListenerToken = forall a. ListenerToken (ForeignPtr (WlListener a))

foreign import ccall unsafe "c_signal_add" c_signal_add :: Ptr (WlSignal a) -> Ptr (WlListener a) -> IO ()
foreign import ccall unsafe "wl_list_init" c_list_init :: Ptr WlList -> IO ()
foreign import ccall unsafe "wl_list_remove" c_list_remove :: Ptr WlList -> IO ()


freeWlListener :: forall a. Ptr (WlListener a) -> IO ()
freeWlListener ptr = do
    let link = #{ptr struct wl_listener, link} ptr
    c_list_remove link
    notify :: FunPtr (Ptr a -> IO ()) <- #{peek struct wl_listener, notify} ptr
    freeHaskellFunPtr notify
    free ptr

foreign import ccall "wrapper" mkCbFun :: (Ptr (WlListener a) -> Ptr a -> IO ()) -> IO (FunPtr (Ptr (WlListener a) -> Ptr a -> IO ()))

makeListenerPtr :: forall a. WlListener a -> IO (ForeignPtr (WlListener a))
makeListenerPtr (WlListener fun) = do
    mem :: Ptr (WlListener a) <- mallocBytes #{size struct wl_listener}
    let link = #{ptr struct wl_listener, link} mem
    c_list_init link
    funPtr <- mkCbFun (\_ -> fun)
    #{poke struct wl_listener, notify} mem funPtr
    newForeignPtr mem (freeWlListener mem)

addListener :: WlListener a -> Ptr (WlSignal a) -> IO (ListenerToken)
addListener listener signal = do
    ptr <- makeListenerPtr listener
    withForeignPtr ptr $ c_signal_add signal
    pure (ListenerToken ptr)

removeListener :: ListenerToken -> IO ()
removeListener (ListenerToken ptr) =
    withForeignPtr ptr removeListener'

removeListener' :: Ptr (WlListener a) -> IO ()
removeListener' ptr =
    let link = #{ptr struct wl_listener, link} ptr
     in c_list_remove link
