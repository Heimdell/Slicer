module Slice where

import Control.Monad (when, forM_, guard)

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Maybe

type Point = (Int, Int, Int)

type Volume a = Map Point a

data Slice a = Slice
    { volume :: Volume a
    , pos    :: Int
    , size   :: Int
    , axis   :: Axis
    }

data Axis = X | Y | Z
    deriving (Enum, Eq)

data Cursor a = Cursor { screen :: Slice a, i, j :: Int }

raytrace :: (Axis, Bool) -> Point -> Volume a -> (Maybe a, Int)
raytrace (ray, toLover) (x, y, z) volume =
    (value, length before)
  where
    value = do
        point <- maybeHead after
        point `Map.lookup` volume

    (before, after) = break (`Map.member` volume) trace

    trace = case ray of
        X -> [(i, y, z) | i <- [x, x + d.. x + d * 4]]
        Y -> [(x, i, z) | i <- [y, y + d.. y + d * 4]]
        Z -> [(x, y, i) | i <- [z, z + d.. z + d * 4]]

    d | toLover   = -1
      | otherwise =  1

maybeHead (x : xs) = Just x
maybeHead []       = Nothing

plane
    :: Axis
    -> Point
    -> (Int, Int, Int, Int)
    -> [[Point]]

plane ray (x, y, z) (u, v, u1, v1) =
    case ray of
        X -> [[(x, i, j) | i <- [u.. u1]] | j <- [v.. v1]]
        Y -> [[(i, y, j) | i <- [u.. u1]] | j <- [v.. v1]]
        Z -> [[(i, j, z) | i <- [u.. u1]] | j <- [v.. v1]]

project
    :: (Axis, Bool)
    -> (Int, Int, Int, Int)
    -> Point
    -> Volume a
    -> ((Point, Maybe a, Int) -> String)
    -> [[String]]

project ray@(axis, _) bounds origin volume drawer =
    ls
  where
    ls = flip map surface $ map $ \point ->
        let
            (shape, depth) = raytrace ray point volume
        in
            drawer (point, shape, depth)

    surface = plane axis origin bounds

dump :: [[String]] -> IO ()
dump = mapM_ (putStrLn . concat)

stdDrawer pt (pt1, x, depth)
    | pt == pt1
        = code' 7 ++ stdDrawer' (x, depth) ++ code' 0
    | otherwise
        = stdDrawer' (x, depth)

stdDrawer' (x, depth) =
    uncurry code (mapping depth) ++ fromMaybe "." x ++ code 0 0
  where
    mapping 5 = (30, 2)
    mapping 4 = (37, 2)
    mapping 3 = (32, 2)
    mapping 2 = (32, 1)
    mapping 1 = (37, 0)
    mapping 0 = (37, 1)

view point bounds volume =
    dump (zipWith (++) (pr X) (pr Y) ++ (pr Z))
  where
    pr axis = project (axis, True) bounds point volume (stdDrawer point)

fromPlanes planes @ (plane @ (line : _) : _) =
    Map.fromList $ do
        x <- [0.. w - 1]
        y <- [0.. h - 1]
        z <- [0.. d - 1]

        let thing = planes !! z !! y !! x

        guard (thing /= ' ')

        return ((x - w `div` 2, y - h `div` 2, z - d `div` 2), [thing])
  where
    w = length line
    h = length plane
    d = length planes

code  x y = "\x1b[" ++ show x ++ ";" ++ show y ++ "m"
code' x   = "\x1b[" ++ show x ++ "m"

testMap :: Volume String
testMap = fromPlanes
    [ [ "     "
      , "  *  "
      , "     "
      ]
    , [ "  *  "
      , "     "
      , "  *  "
      ]
    , [ " *   "
      , "     "
      , "   * "
      ]
    , [ "*    "
      , "     "
      , "    *"
      ]
    , [ "     "
      , "*   *"
      , "     "
      ]
    ]

main = view (0, 0, 0) (-5, -5, 5, 5) testMap
