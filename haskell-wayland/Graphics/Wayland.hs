{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Wayland (
  version, Fixed256, Precision256, Time, Result(..), errToResult,
  diffTimeToTime, timeToDiffTime, ProtocolVersion(..), scannedVersionOf
  ) where

import Foreign.C.Types
import Data.Proxy

import Graphics.Wayland.Internal.Util
import Graphics.Wayland.Internal.Version


data Result = Success | Failure deriving (Eq, Show)
errToResult :: CInt -> Result
errToResult 0    = Success
errToResult (-1) = Failure

class ProtocolVersion a where
  protocolVersion :: Proxy a -> Int

scannedVersionOf :: forall a. (ProtocolVersion a) => a -> Int
scannedVersionOf x = protocolVersion (Proxy :: Proxy a)
