import Control.Concurrent

import Graphics.Wayland.Client

main = do
  connect <- displayConnect
  let display = case connect of
                  Just x -> x
                  Nothing -> error "couldn't connect to a wayland server."
  fd <- displayGetFd display
  putStrLn $ "Using file descriptor " ++ show fd
  putStrLn $ "Display at " ++ show display
  registry <- displayGetRegistry display
  putStrLn $ "Registry at "++ show registry
  let listener = RegistryListener {
    registryGlobal = \reg name ifacename version -> putStrLn $ "Received global " ++ show name ++ " (" ++ ifacename ++ ") version " ++ show version,
    registryGlobalRemove = \ _ _ -> return ()
    }
  errorCode <- registrySetListener registry listener
  putStrLn $ "Setting registry listener... " ++ show errorCode

  res <- displayPrepareRead display
  putStrLn $ "Preparing read... " ++ show res
  flushed <- displayFlush display
  putStrLn $ "Flushed " ++ show flushed
  putStrLn "polling"
  threadWaitRead fd
  putStrLn $ "Ready to read."
  events <- displayReadEvents display
  putStrLn $ "Read display events: " ++ show events
  dispatched <- displayDispatchPending display
  putStrLn $ "Dispatched events: " ++ show dispatched
  displayDisconnect display
