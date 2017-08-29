
import Data.Maybe (listToMaybe)
import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Backend
    ( Backend, backendAutocreate, backendStart
    , BackendSignals(..), backendGetSignals
    )
import Graphics.Wayland.WlRoots.Output (Output, getName, getModes, setOutputMode)
import Graphics.Wayland.WlRoots.Input (InputDevice, inputDeviceType)

import Graphics.Wayland.Server (displayCreate)
import Graphics.Wayland.Signal

import System.IO

data Handlers = Handlers ListenerToken ListenerToken ListenerToken ListenerToken

handleOutputAdd :: Ptr Output -> IO ()
handleOutputAdd output = do
    hPutStrLn stderr "Got an output"
    putStr "Found output: "
    name <- getName output
    putStrLn name

    modes <- getModes output
    putStr "Possible modes: "
    print modes
    case listToMaybe modes of
        Nothing -> pure ()
        Just x -> setOutputMode x output

handleInputAdd :: Ptr InputDevice -> IO ()
handleInputAdd ptr = do
    putStr "Found a new input of type: "
    print =<< inputDeviceType ptr

addSignalHandlers :: Ptr Backend -> IO Handlers
addSignalHandlers ptr =
    let signals = backendGetSignals ptr
     in Handlers
        <$> addListener (WlListener handleInputAdd) (inputAdd signals)
        <*> addListener (WlListener (\_ -> putStrLn "Lost an input")) (inputRemove signals)
        <*> addListener (WlListener handleOutputAdd) (outputAdd signals)
        <*> addListener (WlListener (\_ -> putStrLn "Lost an output")) (outputRemove signals)

main :: IO ()
main = do
    display <- displayCreate
    backend <- backendAutocreate display

    handlers <- addSignalHandlers backend

    backendStart backend
    let Handlers h1 h2 h3 h4 = handlers
    removeListener h1
    removeListener h2
    removeListener h3
    removeListener h4
