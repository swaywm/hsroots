{-# LANGUAGE TupleSections #-}
module ViewSet
where

import Graphics.Wayland.WlRoots.Box (boxContainsPoint, Point)
import View (View, getViewBox)
import Data.IntMap (IntMap)
import Data.Maybe (listToMaybe)
import qualified Data.IntMap as M

type ViewSet = IntMap View

viewBelow :: Point -> ViewSet -> IO (Maybe View)
viewBelow point set = do
    let views = M.elems set

    boxes <- mapM (\v -> (v, ) <$> getViewBox v) views
    let candidates = filter (boxContainsPoint point . snd) boxes
    pure . listToMaybe . map fst $ candidates
