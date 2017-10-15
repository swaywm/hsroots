module Graphics.Wayland.WlRoots.Box
    ( WlrBox (..)
    , Point (..)

    , boxContainsPoint
    )
where

#include <wlr/types/wlr_box.h>

import Foreign.Storable (Storable(..))

data WlrBox = WlrBox 
    { boxX :: Int
    , boxY :: Int
    , boxWidth :: Int
    , boxHeight :: Int
    }

data Point = Point { pointX :: Int, pointY :: Int }

boxContainsPoint :: Point -> WlrBox -> Bool
boxContainsPoint (Point px py) (WlrBox bx by bw bh) =
    bx <= px && px - bx <= bw && by <= py && py - by <= bh

instance Storable WlrBox where
    sizeOf _ = #{size struct wlr_box}
    alignment _ = #{alignment struct wlr_box}
    peek ptr = WlrBox
        <$> #{peek struct wlr_box, x} ptr
        <*> #{peek struct wlr_box, y} ptr
        <*> #{peek struct wlr_box, width} ptr
        <*> #{peek struct wlr_box, height} ptr
    poke ptr box = do
        #{poke struct wlr_box, x} ptr $ boxX box
        #{poke struct wlr_box, y} ptr $ boxY box
        #{poke struct wlr_box, width} ptr $ boxWidth box
        #{poke struct wlr_box, height} ptr $ boxHeight box
