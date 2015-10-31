
{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveFunctor, FlexibleContexts #-}

module ConsoleInteraction where

import Control.Lens
import Control.Monad.State

import Data.Maybe

----

import View
import Scan
import Utils

data Interaction a = Interaction { _ray :: (Axis, Int), _viewport :: View a }

makeLenses ''Interaction

type Interact a = StateT (Interaction a) IO

runInteraction interaction = do
    interaction `evalStateT` Interaction { _ray = (Z, 1), _viewport = View.empty }

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
            let Just (depth, thing) = trace^?places.ix (Place x y)

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
