module Graph.Inductive.Maze where

import Control.Monad (forM_, liftM, mapM, sequence_)
import Control.Monad.Random (MonadRandom, getRandomR, )
import Data.Functor ((<&>))
import Data.Graph.Inductive (Gr, Node, edgeLabel, labEdges)
import qualified Data.Graph.Inductive as Graph
import Data.List ((\\))
import Graph.Inductive.DFS (edfsR, ghead)

data Orientation = Horizontal | Vertical deriving (Show, Eq)

type Weight = Int

data Wall = Wall (Node, Node) Orientation deriving (Eq)

data WeightedWall = WeightedWall (Node, Node, Weight) Orientation deriving (Eq)

instance Show Wall where
  show (Wall (x, y) Vertical) = show (x, y) <> " |"
  show (Wall (x, y) Horizontal) = show (x, y) <> " -"

type Grid = Gr () Wall

grid :: Node -> Node -> Grid
grid width height = Graph.mkGraph nodes edges
  where
    nodes = [(node, ()) | node <- [0 .. width * height - 1]]
    edges =
      [ (n, n', wall n Vertical)
        | (n, _) <- nodes,
          (n', _) <- nodes,
          n - n' == 1 && n `mod` width -1 /= 0
      ]
        <> [ (n, n', wall n Horizontal)
             | (n, _) <- nodes,
               (n', _) <- nodes,
               n - n' == width
           ]
    wall :: Node -> Orientation -> Wall
    wall n =
      let (x, y) = n `divMod` width
       in Wall (x, y)

weightedGrid :: MonadRandom m => Int -> Int -> Gr () (m WeightedWall)
weightedGrid width height = Graph.mkGraph nodes edges
  where
    nodes = [(node, ()) | node <- [0 .. width * height - 1]]
    edges =
      [ (n, n', ww n width Vertical)
        | (n, _) <- nodes,
          (n', _) <- nodes,
          n - n' == 1 && n `mod` width /= 0
      ]
        ++ [ (n, n', ww n width Horizontal)
             | (n, _) <- nodes,
               (n', _) <- nodes,
               n - n' == width
           ]
    ww n width orientation = do
      w <- getRandomR (1, 100) -- random constant
      let (y, x) = n `divMod` width
       in return (WeightedWall (x, y, w) orientation)

generate :: MonadRandom m => Node -> Node -> m [Graph.LEdge Wall]
generate width height =
  let graph = grid width height
   in (Graph.labEdges graph \\) <$> edfsR (ghead graph) graph

generateMST :: MonadRandom m => Node -> Node -> m [Graph.LEdge WeightedWall]
generateMST  = undefined

-- | Generates a random n × m maze.
maze :: MonadRandom m => Int -> Int -> m [Wall]
maze width height =
  generate width height <&> map Graph.edgeLabel

-- | Generates a random n × m maze using a MST algorithm.
mazeMST :: MonadRandom m => Int -> Int -> m [WeightedWall]
mazeMST width height = map Data.Graph.Inductive.edgeLabel <$> generateMST width height


-- map Graph.edgeLabel <$> generate width height
pp :: WeightedWall -> String
pp (WeightedWall (a, b, c) _) = show a ++ " " ++ show b ++ " " ++ show c

test :: IO ()
test = do
  ww <- mapM Data.Graph.Inductive.edgeLabel $ labEdges (weightedGrid 10 10)
  forM_ (map pp ww) putStrLn
