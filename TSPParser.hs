module TSPParser where

import TSP
import Data.Array
import Data.List.Split

parse :: FilePath -> IO TSP
parse fp = do
	rawString <- readFile fp
	let lines = splitOn "\n" rawString
	let points = map lineToPoint lines
	let size = length points
	return (TSP size (listArray (0, size-1) points) (listArray (0, size-1) [0..]))

lineToPoint :: String -> (Double, Double)
lineToPoint string = (read x, read y)
	where
		[_, x, y] = splitOn " " string