
{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveFunctor #-}

import Control.Arrow
import Control.Lens

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Maybe

data Point = Point { _x, _y, _z :: Int }
    deriving (Eq, Ord, Show)

data Place = Place { _row, _col :: Int }
    deriving (Eq, Ord, Show)

data Bounds = Bounds { _u, _v, _u1, _v1 :: Int }
    deriving (Eq, Show)

radial edge = Bounds (-edge) (-edge) edge edge

type Volume a = Map Point a

data Axis = X | Y | Z
    deriving (Enum, Eq)

data View a = View { _point :: Point, _volume :: Volume a, _bounds :: Bounds }
    deriving (Show)

data Scan a = Scan { places :: Map Place a }
    deriving (Show, Functor)

makeLenses ''Point
makeLenses ''Bounds
makeLenses ''View

focus :: Lens' (View a) (Maybe a)
focus act view =
    let thing = view^? volume . ix (view^.point)
    in  every (act thing) $ \it ->
            view & volume %~ Map.alter (const it) (view^.point)

every :: Functor f => f a -> (a -> b) -> f b
every = flip fmap

raytrace (axis, inc) depth view =
    surface `every` (zbuffer depth . trace inc axis)
  where
    surface = plane axis view
    trace inc axis =
        iterate (along axis %~ (+ inc))

    zbuffer depth = count . break isJust . map (\pt -> view^?volume.ix pt) . take depth
    count (empties, rest) =
        (length empties, maybeFirst rest)

maybeFirst list = list^._Cons._1

plane :: Axis -> View a -> Scan Point
plane axis view =
    Scan (Map.fromList places)
  where
    places = viewport `every` \(u, v) ->
        ( Place u v
        , view^.point & ortogonalTo axis . offset .~ (u, v)
        )

    viewport = fromBounds (view ^. bounds)

fromBounds bounds =
    [ (i, j)
    | i <- [bounds^.u.. bounds^.u1]
    , j <- [bounds^.v.. bounds^.v1]
    ]

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

view1 :: View String
view1 = View (Point 0 0 0) (Map.empty) (radial 1)
