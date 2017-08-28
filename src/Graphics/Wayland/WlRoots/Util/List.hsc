module Graphics.Wayland.WlRoots.Util.List
    ( WlrList(..)
    )
where

#include <wlr/util/list.h>

import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))
import Control.Monad (forM)

data WlrList a = WlrList
    { capacity :: Word
    , length :: Word
    , items :: [a]
    }

instance Storable a => Storable (WlrList a) where
    alignment _ = #{alignment list_t}
    sizeOf _ = #{size list_t}
    peek ptr = do
        cap <- #{peek list_t, capacity} ptr
        len <- #{peek list_t, length} ptr
        content <- forM [0.. (fromIntegral cap) - 1] $ \num -> do
            let array = #{ptr list_t, items} ptr
            peekElemOff array num
        pure (WlrList cap len content)
    poke = error "We don't poke lists, sorry"
