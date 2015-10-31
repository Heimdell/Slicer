
{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveFunctor, FlexibleContexts #-}

module Utils where

import Control.Lens

data Axis = X | Y | Z
    deriving (Enum, Eq)

data Point = Point { _x, _y, _z :: Int }
    deriving (Eq, Ord, Show)

makeLenses ''Point

every :: Functor f => f a -> (a -> b) -> f b
every = flip fmap

along :: Axis -> Lens' Point Int
along X = x
along Y = y
along Z = z

ortogonalTo :: Axis -> Lens' Point (Int, Int)
ortogonalTo X = pair y z
ortogonalTo Y = pair x z
ortogonalTo Z = pair x y

offset :: Setter' (Int, Int) (Int, Int)
offset = lens (error "(Int, Int).^offset: cannot get") shift
  where
    shift (x, y) (dx, dy) = (x + dx, y + dy)

pair :: Lens' s a -> Lens' s b -> Lens' s (a, b)
pair first second = lens get put
  where
    get whole = (whole^.first, whole^.second)
    put whole (a, b) = whole
        & first  `set` a
        & second `set` b
