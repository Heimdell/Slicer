
{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveFunctor, FlexibleContexts #-}

import Control.Arrow
import Control.Lens
import Control.Monad.State

import Data.Map (Map, (!))
import qualified Data.Map as Map

import Data.Maybe

import System.IO

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

data View a = View 
    { _point  :: Point
    , _volume :: Volume a
    , _bounds :: Bounds 
    }
    deriving (Show)

newtype Scan a = Scan { _places :: Map Place a }
    deriving (Show, Functor)

makeLenses ''Point
makeLenses ''Bounds
makeLenses ''View
makeLenses ''Scan

focus :: Lens' (View a) (Maybe a)
focus act view =
    let thing = view^? volume . ix (view^.point)
    in  every (act thing) $ \it ->
            view & volume %~ Map.alter (const it) (view^.point)

every :: Functor f => f a -> (a -> b) -> f b
every = flip fmap

--raytrace :: (Axis, Int) -> Int -> View a -> Scan (Int, Maybe a)
raytrace (axis, delta) depth view =
    surface `every` (zbuffer . take depth . trace delta axis)
  where
    surface          = plane axis view
    trace delta axis = iterate (along axis %~ (+ delta))
    zbuffer          = count . break isJust . map (\pt -> view^?volume.ix pt)
    count (empties, rest) = 
        (length empties, maybeFirst rest)

    maybeFirst list  = list^?_Cons._1._Just

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

emptyView :: View String
emptyView = View (Point 0 0 0) (Map.empty) (radial 5)

data Interaction a = Interaction { _ray :: (Axis, Int), _viewport :: View a }

makeLenses ''Interaction

type Interact a = StateT (Interaction a) IO

runInteraction interaction = do
    interaction `evalStateT` Interaction { _ray = (Z, 1), _viewport = emptyView }

interact' :: Interact String ()
interact' = do
    lift $ do
        putStrLn ""
        putStrLn "wsad + qz, space cleans, other sets"
        putStrLn "----"
    
    draw
    c <- lift getChar
    
    case c of
        'w' -> viewport.point.x -= 1
        's' -> viewport.point.x += 1
        'a' -> viewport.point.y -= 1
        'd' -> viewport.point.y += 1
        'z' -> viewport.point.z += 1
        'q' -> viewport.point.z -= 1
        ' ' -> viewport.focus   .= Nothing
        c   -> viewport.focus   .= Just [c]

    when (c /= '\ESC')
        interact'

draw :: Interact String ()
draw = do
    (vol, ray)       <- use (pair viewport ray)
    Bounds u v u1 v1 <- use (viewport.bounds)
    
    let trace = raytrace ray 5 vol

    forM [u.. u1] $ \x -> do
        forM [v.. v1] $ \y -> do
            let Just (depth, thing) = Place x y `Map.lookup` (trace^.places)

            lift $ putStr $ drawPoint (x, y) depth (fromMaybe "." thing)
        
        lift $ putStrLn ""

    return ()

drawPoint :: (Int, Int) -> Int -> String -> String
drawPoint (0, 0) depth cell =
    code' 7 ++ drawPoint (1, 1) depth cell ++ code' 0

drawPoint (x, y) depth cell =
    uncurry code (mapping depth) ++ cell ++ code' 0
  where
    mapping 5 = (30, 2)
    mapping 4 = (30, 1)
    mapping 3 = (30, 1)
    mapping 2 = (37, 2)
    mapping 1 = (37, 0)
    mapping 0 = (37, 1)

code  x y = "\x1b[" ++ show x ++ ";" ++ show y ++ "m"
code' x   = "\x1b[" ++ show x ++ "m"

main = do
    stdin `hSetBuffering` NoBuffering
    stdin `hSetEcho`      False
    
    runInteraction interact'