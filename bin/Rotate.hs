{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.IORef (IORef, readIORef, modifyIORef, newIORef, writeIORef)
import Cat (getCatTexture)
import Graphics.Wayland.WlRoots.Render
    ( Texture
    , Renderer
    , doRender
    , getMatrix
    , renderWithMatrix
    )
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix)
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Output
    ( Output
    , makeOutputCurrent
    , swapOutputBuffers
    , effectiveResolution
    , getTransMatrix
    )
import Graphics.Wayland.WlRoots.Backend (Backend)
import Control.Exception (bracket_)

import Shared

import Control.Monad (forM_)
import Text.XkbCommon.InternalTypes (Keysym, Direction(..))
import Text.XkbCommon.KeysymPatterns


data OutputState = OutputState
    { xOffset :: Int
    , yOffset :: Int
    }

data CatRenderer = CatRenderer (Ptr Renderer) (Ptr Texture)

data RotationState = RotationState CatRenderer Double Double

renderOn :: Ptr Output -> Ptr Renderer -> IO a -> IO a
renderOn output rend act = bracket_ 
    (makeOutputCurrent output)
    (swapOutputBuffers output)
    (doRender rend output act)

frameHandler :: IORef OutputState -> IORef RotationState -> Double -> Ptr Output -> IO ()
frameHandler ref stateRef secs output = do
    state <- readIORef ref
    (RotationState (CatRenderer rend tex) xSpeed ySpeed) <- readIORef stateRef
    (width, height) <- effectiveResolution output
    -- The offsets on the x axis
    let xs = [-128 + xOffset state, xOffset state .. width]
    -- The offsets on the y axis
    let ys = [-128 + yOffset state, yOffset state .. height]
    -- All offsets in the 2 dimensional plane
    let zs = [ (x, y) | x <- xs, y<- ys]

    renderOn output rend $ withMatrix $ \matrix -> forM_ zs $ \(x, y) -> do
        getMatrix tex matrix (getTransMatrix output) (fromIntegral x) (fromIntegral y)
        renderWithMatrix rend tex matrix
    let adjustX = floor $ xSpeed * secs
    let adjustY = floor $ ySpeed * secs

    modifyIORef ref (\(OutputState x y) -> OutputState
        ((x + adjustX) `mod` 128)
        ((y + adjustY) `mod` 128))


getCatRenderer :: Ptr Backend -> IO (CatRenderer)
getCatRenderer backend = do
    renderer <- rendererCreate backend
    texture <- getCatTexture renderer
    pure $ CatRenderer renderer texture

getInitialState :: Ptr Backend -> IO RotationState
getInitialState backend = do
    cat <- getCatRenderer backend
    pure $ RotationState cat 128 128

handleOutputAdd :: IORef RotationState -> Ptr Output -> IO FrameHandler
handleOutputAdd catRef _ = do
    ref <- newIORef (OutputState 0 0)

    pure $ frameHandler ref catRef

handleKeyPress :: IORef RotationState -> Keysym -> Direction -> IO ()
handleKeyPress ref sym (Direction dir) = if dir == 1
    then let fun = case sym of
                Keysym_Up ->    \(RotationState c x y) -> RotationState c x (y - 16)
                Keysym_Down ->  \(RotationState c x y) -> RotationState c x (y + 16)
                Keysym_Right -> \(RotationState c x y) -> RotationState c (x + 16) y
                Keysym_Left ->  \(RotationState c x y) -> RotationState c (x - 16) y
                _ -> id
          in modifyIORef ref fun
    else pure ()

main :: IO ()
main = do
    stateRef <- newIORef undefined

    launchCompositor ignoreHooks
        { backendPostHook = \backend -> writeIORef stateRef =<< getInitialState backend
        , outputAddHook = handleOutputAdd stateRef
          , keyPressHook = handleKeyPress stateRef
        }
