module Graphics.Wayland.WlRoots.Box
    ( WlrBox (..)
    , Point (..)

    , boxContainsPoint
    , centerBox
    , toOrigin
    , shrink
    , enlarge
    )
where

#include <wlr/types/wlr_box.h>

import Foreign.C.Types (CInt(..))
import Foreign.Storable (Storable(..))

data WlrBox = WlrBox 
    { boxX :: Int
    , boxY :: Int
    , boxWidth :: Int
    , boxHeight :: Int
    } deriving (Eq, Show)

data Point = Point { pointX :: Int, pointY :: Int }
    deriving (Eq, Show)

boxContainsPoint :: Point -> WlrBox -> Bool
boxContainsPoint (Point px py) (WlrBox bx by bw bh) =
    bx <= px && px - bx <= bw && by <= py && py - by <= bh

readCInt :: IO CInt -> IO Int
readCInt = fmap fromIntegral

toCInt :: Integral a => a -> CInt
toCInt = fromIntegral

instance Storable WlrBox where
    sizeOf _ = #{size struct wlr_box}
    alignment _ = #{alignment struct wlr_box}
    peek ptr = WlrBox
        <$> readCInt (#{peek struct wlr_box, x} ptr)
        <*> readCInt (#{peek struct wlr_box, y} ptr)
        <*> readCInt (#{peek struct wlr_box, width} ptr)
        <*> readCInt (#{peek struct wlr_box, height} ptr)
    poke ptr box = do
        #{poke struct wlr_box, x} ptr . toCInt $ boxX box
        #{poke struct wlr_box, y} ptr . toCInt $ boxY box
        #{poke struct wlr_box, width} ptr . toCInt $ boxWidth box
        #{poke struct wlr_box, height} ptr . toCInt $ boxHeight box

-- | Center the first argument in the second
-- This doesn't produce an error, but weird results when the box to be centered
-- is bigger than the box to center in!
centerBox :: WlrBox -> WlrBox -> WlrBox
centerBox (WlrBox _ _ innerW innerH) (WlrBox x y outerW outerH) =
    let offX = (outerW - innerW) `div` 2
        offY = (outerH - innerH) `div` 2
     in WlrBox (x + offX) (y + offY) innerW innerH

toOrigin :: WlrBox -> WlrBox
toOrigin (WlrBox {boxWidth = width, boxHeight = height}) = WlrBox 0 0 width height

shrink :: WlrBox -> WlrBox -> WlrBox
shrink (WlrBox _ _ lw lh) (WlrBox _ _ rw rh) = WlrBox 0 0 (min lw rw) (min lh rh)

enlarge :: WlrBox -> WlrBox -> WlrBox
enlarge (WlrBox _ _ lw lh) (WlrBox _ _ rw rh) = WlrBox 0 0 (max lw rw) (max lh rh)
