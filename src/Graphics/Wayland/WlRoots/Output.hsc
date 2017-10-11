{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TupleSections #-}
module Graphics.Wayland.WlRoots.Output
    ( Output
    , outputEnable
    , outputDisable
    , makeOutputCurrent
    , swapOutputBuffers

    , effectiveResolution
    , destroyOutput
    , moveCurosr

    , OutputMode(..)
    , setOutputMode

    , getName
    , getModes
    , getTransMatrix

    , OutputSignals(..)
    , getOutputSignals
    , getDataPtr

    , transformOutput
    , setCursor
    )
where

#include <wlr/types/wlr_output.h>

import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIf_)
import Foreign.C.Types (CInt(..))

import Graphics.Wayland.WlRoots.Util.List (WlrList(..))
import Graphics.Wayland.WlRoots.Render.Matrix (Matrix(..))
import Graphics.Wayland.Signal (WlSignal)
import Graphics.Wayland.Server (OutputTransform(..))

data Output

foreign import ccall unsafe "wlr_output_enable" c_output_enable :: Ptr Output -> Bool -> IO ()

outputEnable :: Ptr Output -> IO ()
outputEnable = flip c_output_enable True

outputDisable :: Ptr Output -> IO ()
outputDisable = flip c_output_enable False


foreign import ccall unsafe "wlr_output_make_current" c_make_current :: Ptr Output -> IO ()
makeOutputCurrent :: Ptr Output -> IO ()
makeOutputCurrent = c_make_current


foreign import ccall unsafe "wlr_output_swap_buffers" c_swap_buffers :: Ptr Output -> IO ()
swapOutputBuffers :: Ptr Output -> IO ()
swapOutputBuffers = c_swap_buffers


foreign import ccall unsafe "wlr_output_destroy" c_output_destroy :: Ptr Output -> IO ()

destroyOutput :: Ptr Output -> IO ()
destroyOutput = c_output_destroy


foreign import ccall unsafe "wlr_output_effective_resolution" c_effective_resolution :: Ptr Output -> Ptr CInt -> Ptr CInt -> IO ()

effectiveResolution :: Ptr Output -> IO (Int, Int)
effectiveResolution output = alloca $ \width -> alloca $ \height -> do
    c_effective_resolution output width height
    width_val <- peek width
    height_val <- peek height
    pure (fromIntegral width_val, fromIntegral height_val)


foreign import ccall unsafe "wlr_output_move_cursor" c_move_cursor :: Ptr Output -> CInt -> CInt -> IO Bool

moveCurosr :: Ptr Output -> Int -> Int -> IO ()
moveCurosr ptr x y =
    throwErrnoIf_ not "moveCursor" $ c_move_cursor ptr (fromIntegral x) (fromIntegral y)

--void wlr_output_transform(struct wlr_output *output,
--		enum wl_output_transform transform);
foreign import ccall unsafe "wlr_output_transform" c_output_transform :: Ptr Output -> CInt -> IO ()

transformOutput :: Ptr Output -> OutputTransform -> IO ()
transformOutput ptr (OutputTransform x) =
    c_output_transform ptr (fromIntegral x)


data OutputMode = OutputMode
    { modeFlags   :: Word32
    , modeWidth   :: Word32
    , modeHeight  :: Word32
    , modeRefresh :: Word32
    }
    deriving (Eq, Show)

instance Storable OutputMode where
    alignment _ = #{alignment struct wlr_output_mode}
    sizeOf _ = #{size struct wlr_output_mode}
    peek ptr = OutputMode
        <$> #{peek struct wlr_output_mode, flags} ptr
        <*> #{peek struct wlr_output_mode, width} ptr
        <*> #{peek struct wlr_output_mode, height} ptr
        <*> #{peek struct wlr_output_mode, refresh} ptr
    poke ptr mode = do
        #{poke struct wlr_output_mode, flags}   ptr $ modeFlags mode
        #{poke struct wlr_output_mode, width}   ptr $ modeWidth mode
        #{poke struct wlr_output_mode, height}  ptr $ modeHeight mode
        #{poke struct wlr_output_mode, refresh} ptr $ modeRefresh mode

foreign import ccall unsafe "wlr_output_set_mode" c_set_mode :: Ptr Output -> Ptr OutputMode -> IO Bool

setOutputMode :: Ptr OutputMode -> Ptr Output -> IO ()
setOutputMode mptr ptr = 
    throwErrnoIf_ not "setOutputMode" $ c_set_mode ptr mptr


getName :: Ptr Output -> IO String
getName = peekCString . #{ptr struct wlr_output, name}

getModes :: Ptr Output -> IO [Ptr OutputMode]
getModes ptr = do
    listptr <- #{peek struct wlr_output, modes} ptr
    wlrlist <- peek listptr
    pure $ items wlrlist

getTransMatrix :: Ptr Output -> Matrix
getTransMatrix = 
    Matrix . #{ptr struct wlr_output, transform_matrix}

data OutputSignals = OutputSignals
    { outSignalFrame :: Ptr (WlSignal ())
    , outSignalResolution :: Ptr (WlSignal ())
    }

getOutputSignals :: Ptr Output -> OutputSignals
getOutputSignals ptr = 
    let frame      = #{ptr struct wlr_output, events.frame} ptr
        resolution = #{ptr struct wlr_output, events.resolution} ptr
     in OutputSignals
         { outSignalFrame = frame
         , outSignalResolution = resolution
         }

getDataPtr :: Ptr Output -> Ptr (Ptr a)
getDataPtr = #{ptr struct wlr_output, data}


foreign import ccall "wlr_output_set_cursor" c_set_cursor :: Ptr Output -> Ptr () -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO Bool

setCursor :: Ptr Output -> Ptr () -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
setCursor output buffer strice width height hotspot_x hotspot_y =
    throwErrnoIf_ not "setCursor" $ c_set_cursor output buffer strice width height hotspot_x hotspot_y
