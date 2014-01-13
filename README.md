Solvers
================

Solver and optimiser framework, written in Haskell.

Supports many kind of nodes for polymorphic solvers.

An A* algorithm might require the Node be Traceable, have a Heuristic function and have a well-defined goal.
Such an algorithm might have the following type signature

aStar :: (TNode n a, HNode n a, GNode n a) => n -> n

More for my own fun and growth than some high performance library. The current Beam algorithm uses vectors but does most of the computations in lists, and so is low performance. Might optimise/port properly in the future.