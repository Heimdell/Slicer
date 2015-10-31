
{-# LANGUAGE TemplateHaskell, RankNTypes, DeriveFunctor, FlexibleContexts #-}

module Scan where

import Control.Lens

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

----

import View
import Utils

newtype Scan a = Scan { _places :: Map Place a }
    deriving (Show, Functor)

data Place = Place { _row, _col :: Int }
    deriving (Eq, Ord, Show)

makeLenses ''Scan

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
