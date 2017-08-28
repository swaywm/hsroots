module Graphics.Wayland.Server (
  -- Expose built-in wayland functions
  module Graphics.Wayland.Internal.Server,
  Client(..),
  -- Expose scanned protocol
  module Graphics.Wayland.Internal.SpliceServer,
  module Graphics.Wayland.Internal.SpliceServerTypes,
  ) where

import Graphics.Wayland.Internal.Server
import Graphics.Wayland.Internal.SpliceServer
import Graphics.Wayland.Internal.SpliceServerTypes
import Graphics.Wayland.Internal.Util (Client(..))
