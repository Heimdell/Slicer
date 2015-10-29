--module Slice where

import Control.Monad (when, forM_, guard)

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Maybe

import System.IO

type Point  = (Int, Int, Int)
type Depth  =  Int
type Bounds = (Int, Int, Int, Int)

type Volume a = Map Point a

data Axis = X | Y | Z
    deriving (Enum, Eq)

raytrace
    ::  Depth
    -> (Axis, Bool)
    ->  Point
    ->  Volume a
    -> (Maybe a, Depth)

raytrace depth (ray, toLover) (x, y, z) volume =
    (value, length before)
  where
    value = do
        point <- maybeHead after
        point `Map.lookup` volume

    (before, after) = break (`Map.member` volume) trace

    trace = case ray of
        X -> [(i, y, z) | i <- [x, x + d.. x + d * (depth - 1)]]
        Y -> [(x, i, z) | i <- [y, y + d.. y + d * (depth - 1)]]
        Z -> [(x, y, i) | i <- [z, z + d.. z + d * (depth - 1)]]

    d | toLover   = -1
      | otherwise =  1

maybeHead (x : xs) = Just x
maybeHead []       = Nothing

plane
    :: Axis
    -> Point
    -> Bounds
    -> [[Point]]

plane ray (x, y, z) (u, v, u1, v1) =
    case ray of
        X -> [[(x, i, j) | i <- [u.. u1]] | j <- [v.. v1]]
        Y -> [[(i, y, j) | i <- [u.. u1]] | j <- [v.. v1]]
        Z -> [[(i, j, z) | i <- [u.. u1]] | j <- [v.. v1]]

project
    :: (Axis, Bool)
    -> Bounds
    -> Point
    -> Volume a
    -> ((Point, Maybe a, Depth) -> String)
    -> [[String]]

project ray@(axis, _) bounds origin volume drawer =
    ls
  where
    ls = flip map surface $ map $ \point ->
        let
            (shape, depth) = raytrace 5 ray point volume
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

view point edge volume =
    dump (zipWith (++) (pr X) (pr Y) ++ (pr Z))
  where
    pr axis = project
        (axis, True)
        (boundsFrom axis point edge)
         point
         volume
        (stdDrawer point)

boundsFrom axis (x, y, z) d = case axis of
    X -> (y - d, z - d, y + d, z + d)
    Y -> (x - d, z - d, x + d, z + d)
    Z -> (x - d, y - d, x + d, y + d)

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

interact' point@(x, y, z) volume = do
    view point 5 volume
    c <- getChar
    print point
    case c of
        'a' -> interact' (x + 1, y, z) volume
        'd' -> interact' (x - 1, y, z) volume
        'w' -> interact' (x, y + 1, z) volume
        's' -> interact' (x, y - 1, z) volume
        'q' -> interact' (x, y, z + 1) volume
        'z' -> interact' (x, y, z - 1) volume
        '\ESC' -> return ()
        ' ' -> interact' point (Map.delete point     volume)
        c   -> interact' point (Map.insert point [c] volume)

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

main = do
    stdin `hSetBuffering` NoBuffering
    stdin `hSetEcho`      False
    interact' (0, 0, 0) testMap
