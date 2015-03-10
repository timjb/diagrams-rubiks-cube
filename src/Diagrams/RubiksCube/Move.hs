module Diagrams.RubiksCube.Move
  ( Move (..)
  , mirror
  , invert
  ) where

-- | A move in <http://ruwix.com/the-rubiks-cube/notation/ standard notation>.
data Move = D | D' | U | U' | L | L' | R | R' | F | F' | B | B'
  deriving (Eq, Show, Read)

-- | The opposite move.
invert :: Move -> Move
invert D  = D'
invert D' = D
invert U  = U'
invert U' = U
invert L  = L'
invert L' = L
invert R  = R'
invert R' = R
invert F  = F'
invert F' = F
invert B  = B'
invert B' = B

-- | Swaps left and right.
mirror :: Move -> Move
mirror D  = D'
mirror D' = D
mirror U  = U'
mirror U' = U
mirror L  = R'
mirror L' = R
mirror R  = L'
mirror R' = L
mirror F  = F'
mirror F' = F
mirror B  = B'
mirror B' = B
