{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Math.RubiksCube.GraphicalModel
  ( Aut
  , Side3X3
  , topLeft, topCenter, topRight
  , middleLeft, middleCenter, middleRight
  , bottomLeft, bottomCenter, bottomRight
  , Col3, top, middle, bottom
  , Row3, left, center, right
  , rotateCW, rotateCCW
  , topRow, middleRow, bottomRow
  , leftCol, centerCol, rightCol
  , Cube, frontSide, backSide, leftSide, rightSide, upSide, downSide
  , Cube3X3, cube
  , rotateLeft, rotateRight
  , moveU, moveD
  , moveL, moveR
  , moveF, moveB
  , move
  ) where

import Control.Lens
import Math.RubiksCube.Move (Move (..))

-- | The type of automorphisms
type Aut a = Iso' a a

data Row3 a =
  Row3 { _left :: a
       , _center :: a
       , _right :: a
       } deriving (Show, Eq, Functor)

makeLenses ''Row3

instance Reversing (Row3 a) where
  reversing (Row3 l c r) = Row3 r c l

data Col3 a =
  Col3 { _top :: a
       , _middle :: a
       , _bottom :: a
       } deriving (Show, Eq, Functor)

makeLenses ''Col3

instance Reversing (Col3 a) where
  reversing (Col3 t m b) = Col3 b m t

data Side3X3 a =
  Side3X3 { _topLeft :: a
          , _topCenter :: a
          , _topRight :: a
          , _middleLeft :: a
          , _middleCenter :: a
          , _middleRight :: a
          , _bottomLeft :: a
          , _bottomCenter :: a
          , _bottomRight :: a
          } deriving (Show, Eq, Functor)

makeLenses ''Side3X3

instance Reversing (Side3X3 a) where
  reversing (Side3X3 tl tc tr ml mc mr bl bc br) =
             Side3X3 br bc bl mr mc ml tr tc tl

rotCW :: Side3X3 a -> Side3X3 a
rotCW (Side3X3 tl tc tr ml mc mr bl bc br) =
       Side3X3 bl ml tl bc mc tc br mr tr

rotCCW :: Side3X3 a -> Side3X3 a
rotCCW (Side3X3 tl tc tr ml mc mr bl bc br) =
        Side3X3 tr mr br tc mc bc tl ml bl

rotateCW :: Aut (Side3X3 a)
rotateCW = iso rotCW rotCCW

rotateCCW :: Aut (Side3X3 a)
rotateCCW = iso rotCCW rotCW

topRow :: Lens' (Side3X3 a) (Row3 a)
topRow = lens getter setter
  where
    getter (Side3X3 tl tc tr _ _ _ _ _ _) = Row3 tl tc tr
    setter (Side3X3 _ _ _ ml mc mr bl bc br) (Row3 tl tc tr) =
      Side3X3 tl tc tr ml mc mr bl bc br

middleRow :: Lens' (Side3X3 a) (Row3 a)
middleRow = lens getter setter
  where
    getter (Side3X3 _ _ _ ml mc mr _ _ _) = Row3 ml mc mr
    setter (Side3X3 tl tc tr _ _ _ bl bc br) (Row3 ml mc mr) =
      Side3X3 tl tc tr ml mc mr bl bc br

bottomRow :: Lens' (Side3X3 a) (Row3 a)
bottomRow = lens getter setter
  where
    getter (Side3X3 _ _ _ _ _ _ bl bc br) = Row3 bl bc br
    setter (Side3X3 tl tc tr ml mc mr _ _ _) (Row3 bl bc br) =
      Side3X3 tl tc tr ml mc mr bl bc br

leftCol :: Lens' (Side3X3 a) (Col3 a)
leftCol = lens getter setter
  where
    getter (Side3X3 tl _ _ ml _ _ bl _ _) = Col3 tl ml bl
    setter (Side3X3 _ tc tr _ mc mr _ bc br) (Col3 tl ml bl) =
      Side3X3 tl tc tr ml mc mr bl bc br

centerCol :: Lens' (Side3X3 a) (Col3 a)
centerCol = lens getter setter
  where
    getter (Side3X3 _ tc _ _ mc _ _ bc _) = Col3 tc mc bc
    setter (Side3X3 tl _ tr ml _ mr bl _ br) (Col3 tc mc bc) =
      Side3X3 tl tc tr ml mc mr bl bc br

rightCol :: Lens' (Side3X3 a) (Col3 a)
rightCol = lens getter setter
  where
    getter (Side3X3 _ _ tr _ _ mr _ _ br) = Col3 tr mr br
    setter (Side3X3 tl tc _ ml mc _ bl bc _) (Col3 tr mr br) =
      Side3X3 tl tc tr ml mc mr bl bc br

{-
      +---+
      | u |
  +---+---+---+---+
  | l | f | r | b |
  +---+---+---+---+
      | d |
      +---+
-}

data Cube a =
  Cube { _frontSide :: a
       , _backSide :: a
       , _leftSide :: a
       , _rightSide :: a
       , _upSide :: a
       , _downSide :: a
       } deriving (Show, Eq, Functor)

rotRight' :: Cube a -> Cube a
rotRight' (Cube f b l r u d) = Cube l r b f u d

rotLeft' :: Cube a -> Cube a
rotLeft' (Cube f b l r u d) = Cube r l f b u d

rotateRight' :: Aut (Cube a)
rotateRight' = iso rotRight' rotLeft'

rotateLeft' :: Aut (Cube a)
rotateLeft' = iso rotLeft' rotRight'

makeLenses ''Cube

newtype Cube3X3 a =
  Cube3X3 { _cube :: Cube (Side3X3 a)
          } deriving (Show, Eq, Functor)

makeLenses ''Cube3X3

cong :: Traversal' s a -> Aut a -> Aut s
cong t i = withIso i $ \f g -> iso (over t f) (over t g)

rotateRight :: Aut (Cube3X3 a)
rotateRight =
  cong cube $ rotateRight'
            . cong upSide rotateCCW
            . cong downSide rotateCW

rotateLeft :: Aut (Cube3X3 a)
rotateLeft = from rotateRight

data Vec4 a = Vec4 a a a a deriving (Show, Eq, Functor)

cycRight :: Vec4 a -> Vec4 a
cycRight (Vec4 a b c d) = Vec4 d a b c

cycLeft :: Vec4 a -> Vec4 a
cycLeft (Vec4 a b c d) = Vec4 b c d a

cycleLeft :: Aut (Vec4 a)
cycleLeft = iso cycLeft cycRight

cycleRight :: Aut (Vec4 a)
cycleRight = iso cycRight cycLeft

type RowsLens a = Lens' (Cube (Side3X3 a)) (Vec4 (Row3 a))
type ColsLens a = Lens' (Cube (Side3X3 a)) (Vec4 (Col3 a))

horizontalRows :: Lens' (Side3X3 a) (Row3 a) -> RowsLens a
horizontalRows rowLens = lens getter setter
  where
    getter (Cube f b l r u d) =
      fmap (view rowLens) (Vec4 f r b l)
    setter (Cube f b l r u d) (Vec4 f' r' b' l') =
      Cube (set rowLens f' f) (set rowLens b' b)
           (set rowLens l' l) (set rowLens r' r) u d

topRows :: RowsLens a
topRows = horizontalRows topRow

_middleRows :: RowsLens a
_middleRows = horizontalRows middleRow

bottomRows :: RowsLens a
bottomRows = horizontalRows topRow

moveU :: Aut (Cube3X3 a)
moveU =
  cong cube $ cong topRows cycleLeft
            . cong upSide rotateCW

moveD :: Aut (Cube3X3 a)
moveD =
  cong cube $ cong bottomRows cycleRight
            . cong downSide rotateCW

verticalCols :: Lens' (Side3X3 a) (Col3 a) -> ColsLens a
verticalCols colLens = lens getter setter
  where
    getter (Cube f b l r u d) =
      Vec4 (f ^. colLens) (u ^. colLens) (b ^. reversed . colLens) (d ^. colLens)
    setter (Cube f b l r u d) (Vec4 f' u' b' d') =
      Cube (set colLens f' f) (set (reversed . colLens) b' b) l r
           (set colLens u' u) (set colLens d' d)

leftCols :: ColsLens a
leftCols = verticalCols leftCol

_centerCols :: ColsLens a
_centerCols = verticalCols centerCol

rightCols :: ColsLens a
rightCols = verticalCols rightCol

moveL :: Aut (Cube3X3 a)
moveL =
  cong cube $ cong leftCols cycleLeft
            . cong leftSide rotateCW

moveR :: Aut (Cube3X3 a)
moveR =
  cong cube $ cong rightCols cycleRight
           . cong rightSide rotateCW

ringCols :: Lens' (Side3X3 a) (Col3 a) -> ColsLens a
ringCols colLens = lens getter setter
  where
    getter (Cube f b l r u d) =
      Vec4 (r ^. colLens) (u ^. rotateCW . colLens)
           (l ^. reversed . colLens) (d ^. rotateCCW . colLens)
    setter (Cube f b l r u d) (Vec4 r' u' l' d') =
      Cube f b (set colLens r' r) (set (reversed . colLens) l' l)
               (set (rotateCW . colLens) u' u) (set (rotateCCW . colLens) d' d)

frontCols :: ColsLens a
frontCols = ringCols leftCol

_betweenCols :: ColsLens a
_betweenCols = ringCols centerCol

backCols :: ColsLens a
backCols = ringCols rightCol

moveF :: Aut (Cube3X3 a)
moveF =
  cong cube $ cong frontCols cycleLeft
            . cong frontSide rotateCW

moveB :: Aut (Cube3X3 a)
moveB =
  cong cube $ cong backCols cycleRight
            . cong backSide rotateCW

move :: Move -> Aut (Cube3X3 a)
move D  = moveD
move D' = from moveD
move U  = moveU
move U' = from moveU
move L  = moveL
move L' = from moveL
move R  = moveR
move R' = from moveR
move F  = moveF
move F' = from moveF
move B  = moveB
move B' = from moveB
