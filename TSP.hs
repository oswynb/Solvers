{-# LANGUAGE MultiParamTypeClasses #-}

module TSP where

import Node

import qualified Data.Vector as V
import Data.Array
import Prelude hiding (Left, Right)

data TSP = TSP {
	size		:: Int,
	vertices 	:: Array Int (Double, Double),
	tour 		:: Array Int Int
}

dSquared :: (Double, Double) -> (Double, Double) -> Double
dSquared (x1, y1) (x2, y2) = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)

d :: (Double, Double) -> (Double, Double) -> Double
d a b = sqrt $ dSquared a b

tspDSquared :: TSP -> Double
tspDSquared (TSP size vertices tour) = foldl (\x y -> x + (dSquared 	(vertices ! (tour ! y))
																		(vertices ! (tour ! (y-1))))) 0 [1..size-1]
tspD:: TSP -> Double
tspD (TSP size vertices tour) = foldl (\x y -> x + (d 	(vertices ! (tour ! y))
														(vertices ! (tour ! (y-1))))) 0 [1..size-1]

instance Node TSP Double where
	f = tspD
	neighbours = V.fromList . tspNeighbours

--Generates all index pairs up to the number of vertices
--E.g. for n = 3 it produces [(0, 1), (0, 2), (1, 2)]
allIndexPairs :: Int -> [(Int, Int)]
allIndexPairs 2 = [(1, 0)]
allIndexPairs n = (zip [0..n-2] (repeat (n-1))) ++ allIndexPairs (n-1)

tspNeighbours :: TSP -> [TSP]
tspNeighbours tsp = map (TSP (size tsp) (vertices tsp)) (map (swap (tour tsp)) (allIndexPairs (size tsp)))

swap :: Array Int Int -> (Int, Int) -> Array Int Int
swap arr (indexA, indexB) = arr // [(indexA, arr ! indexB),(indexB, arr ! indexA)]