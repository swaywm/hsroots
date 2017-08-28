{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.Wayland.Internal.Util (
  CInterface(..), Client(..),

  Fixed256, Precision256,

  Time, millisecondsToTime, timeToMilliseconds, diffTimeToTime, timeToDiffTime
  ) where

import Data.Ratio ((%))
import Data.Time.Clock (DiffTime)
import Data.Fixed (Fixed(..), HasResolution(..), Milli(..))
import Data.Typeable
import Data.Functor
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <wayland-server.h>
#include <wayland-util.h>

{#context prefix="wl"#}


-- | struct wl_interface pointer
{#pointer * interface as CInterface newtype#}



-- | opaque server-side wl_client struct
newtype Client = Client (Ptr Client) deriving (Eq)

-- | 8 bits of precision means a resolution of 256.
data Precision256 = Precision256 deriving (Typeable)
instance HasResolution Precision256 where
  resolution _ = 256
-- | Fixed point number with 8 bits of decimal precision.
--
--   The equivalent of wayland's wl_fixed_t.
type Fixed256 = Fixed Precision256

-- | Represents time in seconds with millisecond precision.
--
--
type Time = Milli

millisecondsToTime :: CUInt -> Time
millisecondsToTime = MkFixed . fromIntegral
timeToMilliseconds :: Time -> CUInt
timeToMilliseconds (MkFixed n) = fromIntegral n

timeToDiffTime :: Time -> DiffTime
timeToDiffTime (MkFixed n) = fromRational (n % 1000)

diffTimeToTime :: DiffTime -> Time
diffTimeToTime = fromRational . toRational
