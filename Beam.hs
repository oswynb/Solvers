module Beam where

import Node
import Util

import Data.Vector as V
import Data.Vector.Algorithms.Intro

showBeam :: (Node n a) => Int -> Int -> n -> Bool -> IO n
showBeam _        0 start _          = return start
showBeam beamSize n start maximising = do
	result <- showBeamIter beamSize n (V.singleton start) comp
	return $ V.head result
	where
		comp = boolToComp maximising

beam :: (Node n a) => Int -> Int -> n -> Bool -> n
beam _        0 start _          = start
beam beamSize n start maximising = V.head $ beamIter beamSize n (singleton start) comp
	where
		comp = boolToComp maximising

boolToComp :: (Node n a) => Bool -> Comparison n
boolToComp True  = maxComp
boolToComp False = minComp

beamIter :: (Node n a) => Int -> Int -> Vector n -> Comparison n -> Vector n
beamIter _        0 start _ = start
beamIter beamSize n start comp = beamIter beamSize (n-1) (takeK comp candidates beamSize) comp
	where
		candidates = V.concatMap neighbours start

--less efficient, as it needs to find the smallest element each iteration
showBeamIter :: (Node n a) => Int -> Int -> Vector n -> Comparison n -> IO (Vector n)
showBeamIter _        0 start _    = return start
showBeamIter beamSize n start comp = do
	let result = beamIter beamSize 1 start comp
	putStrLn $ show $ f (V.minimumBy comp result)
	showBeamIter beamSize (n-1) result comp