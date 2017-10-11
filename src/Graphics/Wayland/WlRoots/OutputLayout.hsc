module Graphics.Wayland.WlRoots.OutputLayout
    ( WlrOutputLayout
    , createOutputLayout
    , destroyOutputLayout

    , WlrOutputLayoutOutput
    , layoutGetOutput
    , layoutAtPos

    , addOutput
    , moveOutput
    , removeOutput

    , outputContainsPoint
    , outputIntersects

    , closestPoint
    , addOutputAuto
    )
where

import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(peek))
import Data.Composition ((.:))

import Graphics.Wayland.WlRoots.Output (Output)

data WlrOutputLayout

foreign import ccall "wlr_output_layout_create" c_layout_create :: IO (Ptr WlrOutputLayout)

createOutputLayout :: IO (Ptr WlrOutputLayout)
createOutputLayout = throwErrnoIfNull "createOutputLayout" c_layout_create


foreign import ccall "wlr_output_layout_destroy" c_layout_destroy :: Ptr WlrOutputLayout -> IO ()

destroyOutputLayout :: Ptr WlrOutputLayout -> IO ()
destroyOutputLayout = c_layout_destroy


data WlrOutputLayoutOutput

foreign import ccall "wlr_output_layout_get" c_layout_get :: Ptr WlrOutputLayout -> Ptr Output -> IO (Ptr WlrOutputLayoutOutput)

layoutGetOutput :: Ptr WlrOutputLayout -> Ptr Output -> IO (Ptr WlrOutputLayoutOutput)
layoutGetOutput = throwErrnoIfNull "layoutGetOutput" .: c_layout_get


foreign import ccall "wlr_output_layout_output_at" c_layout_at :: Ptr WlrOutputLayout -> Double -> Double -> IO (Ptr Output)

layoutAtPos :: Ptr WlrOutputLayout -> Double -> Double -> IO (Maybe (Ptr Output))
layoutAtPos layout x y = do
    ret <- c_layout_at layout x y
    pure $ if ret == nullPtr
        then Nothing
        else Just ret


foreign import ccall "wlr_output_layout_add" c_output_add :: Ptr WlrOutputLayout -> Ptr Output -> CInt -> CInt -> IO ()

addOutput :: Ptr WlrOutputLayout -> Ptr Output -> Int -> Int -> IO ()
addOutput layout output x y =
    c_output_add layout output (fromIntegral x) (fromIntegral y)


foreign import ccall "wlr_output_layout_move" c_output_move :: Ptr WlrOutputLayout -> Ptr Output -> CInt -> CInt -> IO ()

moveOutput :: Ptr WlrOutputLayout -> Ptr Output -> Int -> Int -> IO ()
moveOutput layout output x y =
    c_output_move layout output (fromIntegral x) (fromIntegral y)


foreign import ccall "wlr_output_layout_remove" c_output_remove :: Ptr WlrOutputLayout -> Ptr Output -> IO ()

removeOutput :: Ptr WlrOutputLayout -> Ptr Output -> IO ()
removeOutput layout output =
    c_output_remove layout output

-- TODO: output_cords

foreign import ccall "wlr_output_layout_contains_point" c_contains_point :: Ptr WlrOutputLayout -> Ptr Output -> CInt -> CInt -> IO Bool

outputContainsPoint :: Ptr WlrOutputLayout -> Ptr Output -> Int -> Int -> IO Bool
outputContainsPoint layout output x y = c_contains_point layout output (fromIntegral x) (fromIntegral y)


foreign import ccall "wlr_output_layout_intersects" c_intersects :: Ptr WlrOutputLayout -> Ptr Output -> CInt -> CInt -> CInt -> CInt -> IO Bool

outputIntersects :: Ptr WlrOutputLayout -> Ptr Output -> Int -> Int -> Int -> Int -> IO Bool
outputIntersects layout output x1 y1 x2 y2 = c_intersects layout output (fromIntegral x1) (fromIntegral y1) (fromIntegral x2) (fromIntegral y2)


foreign import ccall "wlr_output_layout_closest_point" c_closest_point :: Ptr WlrOutputLayout -> Ptr Output -> Double -> Double -> Ptr Double -> Ptr Double -> IO ()

closestPoint :: Ptr WlrOutputLayout -> Maybe (Ptr Output) -> Double -> Double -> IO (Double, Double)
closestPoint layout Nothing x y = closestPoint layout (Just nullPtr) x y
closestPoint layout (Just output) x y = alloca $ \xptr -> alloca $ \yptr -> do
    c_closest_point layout output x y xptr yptr
    xret <- peek xptr
    yret <- peek yptr
    pure (xret, yret)

-- TODO: Box

foreign import ccall "wlr_output_layout_add_auto" c_add_auto :: Ptr WlrOutputLayout -> Ptr Output -> IO ()

addOutputAuto :: Ptr WlrOutputLayout -> Ptr Output -> IO ()
addOutputAuto = c_add_auto
