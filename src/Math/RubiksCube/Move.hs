module Math.RubiksCube.Move
  ( Move (..)
  ) where

data Move = D | D' | U | U' | L | L' | R | R' | F | F' | B | B'
  deriving (Eq, Show, Read)