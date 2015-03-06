module Main where

import Math.RubiksCube.Draw
import Math.RubiksCube.Move

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main :: IO ()
main = mainWith (tutorial # pad 1.1)
  where cube = solvedCube3X3
        --foldMap = drawFoldMap3X3 cube :: Diagram B R2
        --threeD  = drawMove F off cube :: Diagram B R2
        moves = [F,R,U,R',U',F']
        tutorial = drawMoves with cube moves :: Diagram B R2
