{-# LANGUAGE EmptyDataDecls #-}
module Graphics.Wayland.List
    ( WlList
    , getListFromHead
    , istListEmpty
    )
where

#include <wayland-util.h>

import Foreign.Storable (peekByteOff)
import Foreign.Ptr (Ptr, plusPtr)

import System.IO

data WlList

getListElems' :: Ptr WlList -> Ptr WlList -> IO [Ptr WlList]
getListElems' listHead current
    | listHead == current = pure []
    | otherwise = do
        nxt <- #{peek struct wl_list, next} current
        (current :) <$> getListElems' listHead nxt


getListElems :: Ptr WlList -> IO [Ptr WlList]
getListElems listHead = do
    nxt <- #{peek struct wl_list, next} listHead
    getListElems' listHead nxt

istListEmpty :: Ptr WlList -> IO Bool
istListEmpty ptr = (==) ptr <$> #{peek struct wl_list, next} ptr

getListFromHead :: Ptr WlList -> Word -> IO [Ptr a]
getListFromHead listHead offset =
    map (flip plusPtr (negate $ fromIntegral offset)) <$> getListElems listHead
