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


data OutputState = OutputState
    { xOffset :: Int
    , yOffset :: Int
    }

data CatRenderer = CatRenderer (Ptr Renderer) (Ptr Texture)

renderOn :: Ptr Output -> Ptr Renderer -> IO a -> IO a
renderOn output rend act = bracket_ 
    (makeOutputCurrent output)
    (swapOutputBuffers output)
    (doRender rend output act)

frameHandler :: IORef OutputState -> IORef CatRenderer -> Double -> Ptr Output -> IO ()
frameHandler ref catRef secs output = do
    state <- readIORef ref
    (CatRenderer rend tex) <- readIORef catRef
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
    let adjust = floor $ 128 * secs

    modifyIORef ref (\(OutputState x y) -> OutputState ((x + adjust) `mod` 128) ((y + adjust) `mod` 128))


getCatRenderer :: Ptr Backend -> IO (CatRenderer)
getCatRenderer backend = do
    renderer <- rendererCreate backend
    texture <- getCatTexture renderer
    pure $ CatRenderer renderer texture

handleOutputAdd :: IORef CatRenderer -> Ptr Output -> IO FrameHandler
handleOutputAdd catRef _ = do
    ref <- newIORef (OutputState 0 0)

    pure $ frameHandler ref catRef

main :: IO ()
main = do
    catRef <- newIORef undefined

    launchCompositor ignoreHooks
        { backendPostHook = \backend -> writeIORef catRef =<< getCatRenderer backend
        , outputAddHook = handleOutputAdd catRef
        }
