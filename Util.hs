module Util where

import Node

import Data.Vector as V
import Data.List   as L

data Queue a = Queue {
	inQueue 	:: [a],
	outQueue 	:: [a]
}

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs outQ) = Queue (x:xs) outQ

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue inQ []) = dequeue (Queue [] (L.reverse inQ))
dequeue (Queue inQ (x:xs)) = (x, Queue inQ xs)

--Temporary n lg n takeK
takeK :: (Node n a) => (n -> n -> Ordering) -> Vector n -> Int -> Vector n
takeK comp vec k = V.fromListN k (sortBy comp (toList vec))