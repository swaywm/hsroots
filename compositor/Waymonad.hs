{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Waymonad
    ( WayStateRef
    , WayState

    , get
    , modify
    , runWayState

    , viewBelow
    )
where

import Graphics.Wayland.WlRoots.Box (Point)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), MonadReader, ask)
import Data.IORef (IORef, modifyIORef, readIORef)

-- import Data.IntMap (IntMap)
import View (View)
import qualified ViewSet as VS


-- All of this makes for a fake `Monad State` in IO
-- We need this because we run into callbacks *a lot*.
-- We have to preserve/modify state around those, which cannot be
-- done with the normal StateT (since we exit our Monad-Stack all the time)
-- but we are in IO, which can be abused with this trick.
-- It should all be hidden in the high level apis, low level APIs will
-- require the get and runWayState around callbacks that are IO
type WayStateRef = IORef VS.ViewSet

newtype WayState a = WayState (ReaderT WayStateRef IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader WayStateRef)

get :: (MonadReader (IORef a) m, MonadIO m) => m a
get = liftIO . readIORef =<< ask

modify :: (MonadReader (IORef a) m, MonadIO m) => (a -> a) -> m ()
modify fun = do
    ref <- ask
    liftIO $ modifyIORef ref fun

runWayState :: WayState a -> WayStateRef -> IO a
runWayState (WayState m) ref = runReaderT m ref

viewBelow :: Point -> WayState (Maybe View)
viewBelow point = liftIO . (VS.viewBelow point) =<< get
