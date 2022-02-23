{-# LANGUAGE ViewPatterns #-}

module Graph.Inductive.DFS where

import Data.List(length, splitAt)
import Data.Vector.Mutable(read, write)
import Data.Vector (Vector, fromList, toList, modify)
import Control.Monad.Random (MonadRandom, getRandomR)
import Data.Graph.Inductive
  ( Gr,
    LEdge,
    Node,
    lpre',
    match,
    matchAny,
    neighbors',
    node',
    (&),
  )
import qualified Data.Graph.Inductive as Graph

mapNodes :: (n -> n') -> Gr n e -> Gr n' e
mapNodes _ g | Graph.isEmpty g = Graph.empty
mapNodes f (matchAny -> ((in', node, label, out), g)) =
  (in', node, f label, out) & mapNodes f g

{-
In fgl, Context is just a tuple with four elements: incoming edges, the node, the node label and outgoing edges.

The important idea here is that we don’t need to explicitly keep track of which nodes we’ve visited—after we visit a node,
we always recurse on the rest of the graph which does not contain it
-}

-- |
-- build up a list of visited nodes
dfs :: Graph.Node -> Gr a b -> [Graph.Node]
dfs start  = go [start]
  where
    go [] _ = []
    go _ g | Graph.isEmpty g = []
    go (n : ns) (Graph.match n -> (Just c, g)) =
      n : go (Graph.neighbors' c <> ns) g
    go (_ : ns) g = go ns g

edfs :: Graph.Node -> Gr n e -> [Graph.LEdge e]
edfs start (match start -> (Just ctx, graph)) =
  normalize $ go (lNeighbor' ctx) graph
  where
    go [] _ = []
    go ((p, n, l) : ns) (match n -> (Just c, g)) =
      (p, n, l) : go (lNeighbor' c <> ns) g
    go (_ : ns) g = go ns g

edfsR :: MonadRandom m => Graph.Node -> Gr n e -> m[Graph.LEdge e]
edfsR start (match start -> (Just ctx, graph)) =
  normalize <$> go (lNeighbor' ctx) graph
  where
    go [] _ = pure []
    go  _ g | Graph.isEmpty g  = pure []
    go ((p, n, l) : ns) (match n -> (Just c, g)) =
      do
        edges <- shuffle $ lNeighbor' c
        ( (p, n, l) : ) <$> go (edges <> ns) g
    go (_ : ns) g = go ns g


lNeighbor' :: Graph.Context n e -> [Graph.LEdge e]
lNeighbor' c = [(p, n, l) | (n, l) <- Graph.lpre' c <> Graph.lsuc' c]
  where
    p = Graph.node' c

normalize :: [Graph.LEdge e] -> [Graph.LEdge e]
normalize = map swap
  where
    swap (n, n', l)
      | n < n' = (n', n, l)
      | otherwise = (n, n', l)

shuffle :: MonadRandom m => [a] -> m[a]
shuffle [] =  pure []
shuffle ls  =  do
  (x, xs) <- choose ls
  (x :) <$> shuffle xs
  where
    choose :: MonadRandom m => [a] -> m (a, [a])
    choose  [] = error "list can not be empty"
    choose  ls = do
      i <- getRandomR (0, length ls - 1)
      let (as, x:bs) = splitAt i ls
      return (x, as <> bs)
      
doSwaps :: MonadRandom m => Int -> Vector a -> m (Vector a)
doSwaps 1 vec = return vec
doSwaps len vec =
  let
    swap a b v = do
      tempA <- Data.Vector.Mutable.read v a
      tempB <- Data.Vector.Mutable.read v b
      Data.Vector.Mutable.write v a tempB
      Data.Vector.Mutable.write v b tempA
  in do
    i <- getRandomR(1,len)
    doSwaps (len - 1) $ modify (swap len i) vec
 
-- | Durstenfeld's modification of the Fisher–Yates shuffle runs in O(n) time!
-- We convert ls into an Array arr first for fast indexing.
shuffle2 :: MonadRandom m => [t] -> m [t]
shuffle2 [] = return []
shuffle2 ls = fmap toList (doSwaps (Data.List.length ls) (fromList ls))

ghead :: Gr a b  -> Node
ghead  graph | Graph.isEmpty  graph  = error "Empty graph"
ghead  (matchAny -> ( (_, node, _, _), graph)) = node

sequenceLEdge :: Functor f => Graph.LEdge (f a) ->  f (Graph.LEdge a)
sequenceLEdge  = undefined
