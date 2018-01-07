-- This should probably live in another module in future
module Graphics.Pixman
    ( pixmanRegionExtents
    , PixmanRegion32
    , PixmanBox32 (..)
    )
where

#include <pixman-1/pixman.h>

import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

data PixmanRegion32

data PixmanBox32 = PixmanBox32
    { pBoxX1 :: Int32
    , pBoxY1 :: Int32
    , pBoxX2 :: Int32
    , pBoxY2 :: Int32
    } deriving (Show, Eq)

instance Storable PixmanBox32 where
    sizeOf _ = #{size struct pixman_box32}
    alignment _ = #{alignment struct pixman_box32}
    peek ptr = PixmanBox32
        <$> #{peek struct pixman_box32, x1} ptr
        <*> #{peek struct pixman_box32, y1} ptr
        <*> #{peek struct pixman_box32, x2} ptr
        <*> #{peek struct pixman_box32, y2} ptr
    poke ptr (PixmanBox32 x1 y1 x2 y2) = do
        #{poke struct pixman_box32, x1} ptr x1
        #{poke struct pixman_box32, y1} ptr y1
        #{poke struct pixman_box32, x2} ptr x2
        #{poke struct pixman_box32, y2} ptr y2

foreign import ccall unsafe "pixman_region32_extents" c_32_extends :: Ptr PixmanRegion32 -> IO (Ptr PixmanBox32)

pixmanRegionExtents :: Ptr PixmanRegion32 -> IO PixmanBox32
pixmanRegionExtents ptr = peek =<< c_32_extends ptr
