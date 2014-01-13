{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Node where

import Data.Vector

--Show is so the objective value of a node can be displayed
class (Ord a, Show a) => Node n a | n -> a where
	f :: n -> a
	neighbours :: n -> Vector n

--Heuristic Node, for heuristic algorithms
class (Node n a) => HNode n a where
	h :: n -> a

--Traceable Node, for parent tracing
class (Node n a) => TNode n a where
	parent :: n -> Maybe n

--Goal Node, for problems with a well-defined goal node
class (Node n a) => GNode n a where
	isGoal :: n -> Bool

--Show Node, for use with algorithms which show partial results during run time
class (Show n, Node n a) => SNode n a

maxComp :: (Node n a) => n -> n -> Ordering
maxComp a b = compare (f b) (f a)

minComp :: (Node n a) => n -> n -> Ordering
minComp a b = compare (f a) (f b)