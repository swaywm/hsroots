module Graphics.Wayland.Client (
  -- Expose built-in wayland functions
  module Graphics.Wayland.Internal.Client,
  module Graphics.Wayland.Internal.Cursor,
  module Graphics.Wayland.Internal.EGL,
  -- Expose scanned protocol
  module Graphics.Wayland.Internal.SpliceClient,
  module Graphics.Wayland.Internal.SpliceClientTypes,
  ) where

import Graphics.Wayland.Internal.Client
import Graphics.Wayland.Internal.SpliceClient
import Graphics.Wayland.Internal.SpliceClientTypes
import Graphics.Wayland.Internal.Cursor
import Graphics.Wayland.Internal.EGL
