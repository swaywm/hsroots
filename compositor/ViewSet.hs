{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
module ViewSet
where

import Data.Typeable
import Data.Text (Text)
import Graphics.Wayland.WlRoots.Box (boxContainsPoint, Point, WlrBox)
import View (View, getViewBox)
import Data.Maybe (listToMaybe)

import Data.Foldable (toList)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Map (Map)
import qualified Data.Map as M

type ViewSet = IntMap View

viewBelow :: Traversable t => Point -> t View -> IO (Maybe View)
viewBelow point views = do
    boxes <- mapM (\v -> (v, ) <$> getViewBox v) views
    let candidates = filter (boxContainsPoint point . snd) $ toList boxes
    pure . listToMaybe . map fst $ candidates

class (Show a, Eq a, Ord a) => WSTag a where
    getName :: a -> Text

instance WSTag Text where
    getName = id

data StackSet a = StackSet (Map a [View])

class Layout a where
    pureLayout :: a -> WlrBox -> [b] -> [(b, WlrBox)]
    handleMessage :: a -> SomeMessage -> Maybe a
    description :: a -> Text

class Typeable m => Message m

data SomeMessage = forall m. Message m => SomeMessage m

data Full = Full

instance Layout Full where
    description _ = "Full"
    handleMessage _ _ = Nothing
    pureLayout _ _ [] = []
    pureLayout _ box (x:_) = [(x, box)]


data Zipper a = Zipper
    { zipPre :: [a]
    , zipFoc :: !a
    , zipPos :: [a]
    } deriving (Eq, Show)

instance Functor Zipper where
    fmap f (Zipper pre foc pos) = Zipper (fmap f pre) (f foc) (fmap f pos)

instance Foldable Zipper where
    foldr fun start (Zipper pre foc pos) =
        let right = foldr fun start pos
            middle = foc `fun` right
         in foldr fun middle pre
    foldl fun start (Zipper pre foc pos) =
        let left = foldl fun start pre
            middle = left `fun` foc
         in foldl fun middle pos

instance Traversable Zipper where
    traverse :: Applicative f => (a -> f b) -> Zipper a -> f (Zipper b)
    traverse fun (Zipper pre foc pos) =
        let left = traverse fun pre
            middle = fun foc
            right = traverse fun pos
         in Zipper <$> left <*> middle <*> right
    mapM :: Monad m => (a -> m b) -> Zipper a -> m (Zipper b)
    mapM fun (Zipper pre foc pos) = do
        left <- mapM fun pre
        middle <- fun foc
        right <- mapM fun pos
        pure $ Zipper left middle right
