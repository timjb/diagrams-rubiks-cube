module Math.RubiksCube.Move
  ( Move (..)
  , mirrored
  , inverse
  ) where

data Move = D | D' | U | U' | L | L' | R | R' | F | F' | B | B'
  deriving (Eq, Show, Read)

inverse :: Move -> Move
inverse D  = D'
inverse D' = D
inverse U  = U'
inverse U' = U
inverse L  = L'
inverse L' = L
inverse R  = R'
inverse R' = R
inverse F  = F'
inverse F' = F
inverse B  = B'
inverse B' = B

mirrored :: Move -> Move
mirrored D  = D'
mirrored D' = D
mirrored U  = U'
mirrored U' = U
mirrored L  = R'
mirrored L' = R
mirrored R  = L'
mirrored R' = L
mirrored F  = F'
mirrored F' = F
mirrored B  = B'
mirrored B' = B
