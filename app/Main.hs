module Main where

import Graph.Inductive.Draw

main  :: IO ()
main =  genPng defaults "/tmp/maze.png" 100 200
