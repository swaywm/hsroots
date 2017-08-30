{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Wayland.WlRoots.Util.List
    ( WlrList(..)
    )
where

#include <wlr/util/list.h>

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..))
import Control.Monad (forM)

data WlrList a = WlrList
    { capacity :: Word
    , length :: Word
    , items :: [Ptr a]
    }

instance Storable a => Storable (WlrList a) where
    alignment _ = #{alignment list_t}
    sizeOf _ = #{size list_t}
    peek ptr = do
        cap <- #{peek list_t, capacity} ptr
        len <- #{peek list_t, length} ptr
        let array :: Ptr (Ptr (Ptr a)) = #{ptr list_t, items} ptr
        lptr :: Ptr (Ptr a) <- peek array
        content <- forM [0.. (fromIntegral len) - 1] $ \num -> do
            peekElemOff lptr num
        pure (WlrList cap len content)
    poke = error "We don't poke lists, sorry"
