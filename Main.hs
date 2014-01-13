module Main where

import TSP
import TSPParser
import Beam
import Data.Array

startTSP :: TSP
startTSP = TSP 4 (listArray (0,3) [(0,0), (1,0), (0,1), (1,1)]) (listArray (0,3) [0, 2, 1, 3])

main :: IO ()
main = do
	testTSP <- TSPParser.parse "tsp1" -- taken from http://www.math.uwaterloo.ca/tsp/vlsi/index.html#XQF131
	result <- showBeam 10 100 testTSP False
	putStrLn $ show $ tspD result