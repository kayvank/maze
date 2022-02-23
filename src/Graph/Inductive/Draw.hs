module Graph.Inductive.Draw where

import Graphics.Rendering.Cairo(Render)
import qualified Graphics.Rendering.Cairo as Cairo
import Control.Monad (forM_)
import Graph.Inductive.Maze

data Config = Config
  { -- | size of each cell
    step :: Int,
    -- | how wide the wall is
    wall :: Int,
    -- | how wide the canvas is
    width :: Int,
    -- | how high the canvas is
    height :: Int
  }

-- | Settings that look okay, producing reasonably sized images for
--   40 × 40 mazes.
defaults :: Config
defaults =
  Config
    { step = 10,
      wall = 2,
      width = 404,
      height = 404
    }

-- | This draws a rectangle, coercing ints into doubles.
rectangle :: (Integral n) => n -> n -> n -> n -> Render ()
rectangle x y width height =
  Cairo.rectangle
    (fromIntegral x)
    (fromIntegral y)
    (fromIntegral width)
    (fromIntegral height)


toWall :: WeightedWall -> Wall
toWall (WeightedWall (x, y, _) o) = Wall (x,y) o

renderMaze :: Config -> [Wall] -> Render ()
renderMaze Config {..} walls = do
  Cairo.setSourceRGB 0.02 0.24 0.54
  Cairo.setLineWidth 5

  rectangle 0 0 width height
  Cairo.stroke

  forM_ walls $ \ (Wall (x, y) direction ) -> case direction of
    Horizontal ->  rectangle (x * step) (y * step) (step + wall) wall >> Cairo.fill
    Vertical -> rectangle (x * step) (y * step) wall (step + wall) >> Cairo.fill

-- | Export a maze, given as a list of walls, to PNG.
mazePng :: Config -> FilePath -> [Wall] -> IO ()
mazePng config@Config{..}  file walls =
  Cairo.withImageSurface Cairo.FormatARGB32 width height  $ \surface -> do
     Cairo.renderWith surface $ renderMaze config walls
     Cairo.surfaceWriteToPNG surface file

-- | Generate a maze image with a maze of the given dimensions.
genPng :: Config -> FilePath -> Int -> Int -> IO ()
genPng config file width height = do
  maze <- maze width height
  mazePng config file maze

  -- | Generate a maze image with a maze of the given dimensions.
genPng2 :: Config -> FilePath -> Int -> Int -> IO ()
genPng2 config file width height = do
  maze <- map toWall <$> mazeMST width height
  mazePng config file maze
