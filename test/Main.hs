{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Maze

prop_test :: Property
prop_test = property $ do
  doMaze === "Maze"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
