module Main
where

import Foreign.Ptr (Ptr)
import Data.IORef (newIORef, IORef, writeIORef, readIORef)

import Graphics.Wayland.Resource (resourceDestroy)
import Graphics.Wayland.WlRoots.Render.Matrix (withMatrix, matrixTranslate)
import Graphics.Wayland.WlRoots.Render
    ( Renderer
    , doRender
    , isTextureValid
    , renderWithMatrix
    )
import Graphics.Wayland.WlRoots.Backend (Backend)
import Graphics.Wayland.WlRoots.Render.Gles2 (rendererCreate)
import Graphics.Wayland.WlRoots.Compositor (WlrCompositor, compositorCreate)
import Graphics.Wayland.WlRoots.Shell
    ( WlrShell
    , shellCreate
    , shellSurfaceGetSurface
    , getShellSurfaces
    )
import Graphics.Wayland.WlRoots.XdgShell
    ( WlrXdgShell
    , xdgShellCreate
    , xdgSurfaceGetSurface
    , xdgShellGetSurfaces
    )
import Graphics.Wayland.WlRoots.XWayland
    ( XWayland
    , xwaylandCreate
    , getXWindows
    , x11WindowGetSurface
    )
import Graphics.Wayland.WlRoots.DeviceManager (WlrDeviceManager, managerCreate)
import Graphics.Wayland.WlRoots.Output
    ( Output
    , makeOutputCurrent
    , swapOutputBuffers
    , getTransMatrix
    )
import Graphics.Wayland.WlRoots.Surface
    ( WlrSurface
    , flushDamage
    , surfaceGetTexture
    , withSurfaceMatrix
    , callbackGetResource
    , surfaceGetCallbacks
    , callbackGetCallback
    )
import Graphics.Wayland.Server (displayInitShm, DisplayServer, callbackDone)

import Control.Exception (bracket_)
import Control.Monad (void, when)
import Shared

renderOn :: Ptr Output -> Ptr Renderer -> IO a -> IO a
renderOn output rend act = bracket_ 
    (makeOutputCurrent output)
    (swapOutputBuffers output)
    (doRender rend output act)

data Compositor = Compositor
    { compDisplay :: DisplayServer
    , compRenderer :: Ptr Renderer
    , compCompositor :: Ptr WlrCompositor
    , compShell :: Ptr WlrShell
    , compXdg :: Ptr WlrXdgShell
    , compManager :: Ptr WlrDeviceManager
    , compXWayland :: Ptr XWayland
    }

outputHandleSurface :: Compositor -> Double -> Ptr Output -> Ptr WlrSurface -> IO ()
outputHandleSurface comp secs output surface = do
    flushDamage surface
    texture <- surfaceGetTexture surface
    isValid <- isTextureValid texture
    when isValid $ withMatrix $ \trans -> do
        matrixTranslate trans 200 200 0
        withSurfaceMatrix surface (getTransMatrix output) trans $
            renderWithMatrix (compRenderer comp) texture
        -- Add the callback calling
        callbacks <- surfaceGetCallbacks surface
        cbs <- mapM callbackGetCallback callbacks
        mapM_ (flip callbackDone (floor $ secs * 1000)) cbs
        ress <- mapM callbackGetResource callbacks
        mapM_ resourceDestroy ress


frameHandler :: IORef Compositor -> Double -> Ptr Output -> IO ()
frameHandler compRef secs output = do
    comp <- readIORef compRef
    renderOn output (compRenderer comp) $ do
        -- First build the list of surface we can draw
        shell <- mapM shellSurfaceGetSurface =<< (getShellSurfaces $ compShell comp)
        xdgShell <- mapM xdgSurfaceGetSurface =<< (xdgShellGetSurfaces $ compXdg comp)
        x11 <- mapM x11WindowGetSurface =<< (getXWindows $ compXWayland comp)

        let surfaces = shell ++ xdgShell ++ x11
        mapM_ (outputHandleSurface comp secs output) surfaces



makeCompositor :: DisplayServer -> Ptr Backend -> IO Compositor
makeCompositor display backend = do
    void $ displayInitShm display
    renderer <- rendererCreate backend
    comp <- compositorCreate display backend
    shell <- shellCreate display
    xdgShell <- xdgShellCreate display
    devManager <- managerCreate display
    xway <- xwaylandCreate display comp
    pure $ Compositor display renderer comp shell xdgShell devManager xway

main :: IO ()
main = do
    dpRef <- newIORef undefined
    compRef <- newIORef undefined
    launchCompositor ignoreHooks
        { displayHook = writeIORef dpRef
        , backendPostHook = \backend -> do
            dsp <- readIORef dpRef
            writeIORef compRef =<< makeCompositor dsp backend
        , outputAddHook = \_ -> pure $ frameHandler compRef
        }
    pure ()
