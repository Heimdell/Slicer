
{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveFunctor, FlexibleContexts #-}

module View where

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map

----

import Utils

data View a = View 
    { _point  :: Point
    , _volume :: Volume a
    , _bounds :: Bounds 
    }
    deriving (Show)

data Bounds = Bounds { _u, _v, _u1, _v1 :: Int }
    deriving (Eq, Show)

radial edge = Bounds (-edge) (-edge) edge edge

type Volume a = Map Point a

makeLenses ''Bounds
makeLenses ''View

focus :: Lens' (View a) (Maybe a)
focus act view =
    let thing = view^? volume . ix (view^.point)
    in  every (act thing) $ \it ->
            view & volume %~ Map.alter (const it) (view^.point)

empty :: View String
empty = View (Point 0 0 0) (Map.empty) (radial 5)

fromBounds bounds =
    [ (i, j)
    | i <- [bounds^.u.. bounds^.u1]
    , j <- [bounds^.v.. bounds^.v1]
    ]
