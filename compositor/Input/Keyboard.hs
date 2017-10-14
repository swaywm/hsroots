module Input.Keyboard
where

import Foreign.Ptr (Ptr)
import Graphics.Wayland.WlRoots.Input.Keyboard (WlrKeyboard)

data Keyboard = Keyboard
    { keyboardDevice :: Ptr WlrKeyboard

    }


--handleKeyPress :: CompHooks -> DisplayServer -> Ptr Backend -> Ptr WlrKeyboard -> Ptr EventKey -> IO ()
--handleKeyPress hooks dsp backend keyboard ptr = do
--    event <- peek ptr
--    let keycode = fromEvdev . fromIntegral . keyCode $ event
--    keyState <- getKeystate keyboard
--    syms <- getStateSymsI keyState keycode
--    let keyDir = (keyStateToDirection $ state event)
--    hPutStrLn stderr . intercalate "," $ map keysymName syms
--    forM_ syms $ \sym -> case sym of
--        Keysym_Escape -> displayTerminate dsp
--        -- Would be cooler if this wasn't a listing of VTs (probably TH)
--        Keysym_XF86Switch_VT_1  -> switchVT backend 1
--        Keysym_XF86Switch_VT_2  -> switchVT backend 2
--        Keysym_XF86Switch_VT_3  -> switchVT backend 3
--        Keysym_XF86Switch_VT_4  -> switchVT backend 4
--        Keysym_XF86Switch_VT_5  -> switchVT backend 5
--        Keysym_XF86Switch_VT_6  -> switchVT backend 6
--        Keysym_XF86Switch_VT_7  -> switchVT backend 7
--        Keysym_XF86Switch_VT_8  -> switchVT backend 8
--        Keysym_XF86Switch_VT_9  -> switchVT backend 9
--        Keysym_XF86Switch_VT_10 -> switchVT backend 10
--        Keysym_XF86Switch_VT_11 -> switchVT backend 11
--        Keysym_XF86Switch_VT_12 -> switchVT backend 12
--        _ -> pure ()
--
--handleKeyboardAdd :: CompHooks -> DisplayServer -> Ptr Backend -> Ptr WlrKeyboard -> IO ()
--handleKeyboardAdd hooks dsp backend ptr = do
--    let signals = getKeySignals ptr
--
--    (Just cxt) <- newContext defaultFlags
--    (Just keymap) <- newKeymapFromNamesI cxt noPrefs
--
--    setKeymap ptr keymap
--
--    handler <- addListener (WlListener $ handleKeyPress hooks dsp backend ptr) (keySignalKey signals)
--    sptr <- newStablePtr handler
--    poke (getKeyDataPtr ptr) (castStablePtrToPtr sptr)
--
--
--handleKeyboardRemove :: Ptr WlrKeyboard -> IO ()
--handleKeyboardRemove ptr = do
--    sptr <- peek (getKeyDataPtr ptr)
--    freeStablePtr $ castPtrToStablePtr sptr
