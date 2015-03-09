{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Diagrams.RubiksCube.Model
  ( Aut
  , Row3 (Row3), left, center, right
  , Col3 (Col3), top, middle, bottom
  , Side3X3 (Side3X3)
  , topLeft, topCenter, topRight
  , middleLeft, middleCenter, middleRight
  , bottomLeft, bottomCenter, bottomRight
  , rotateCW, rotateCCW
  , topRow, middleRow, bottomRow
  , leftCol, centerCol, rightCol
  , Cube (Cube), frontSide, backSide, leftSide, rightSide, upSide, downSide
  , Cube3X3 (Cube3X3), cube
  , rotateLeft, rotateRight
  , rotateDown, rotateUp
  , moveU, moveD
  , moveL, moveR
  , moveF, moveB
  , move, doMoves, undoMoves
  , upLayer, middleLayer, downLayer
  , centerFields, cornerFields
  ) where

import Control.Lens
import Diagrams.RubiksCube.Move (Move (..))
import Data.Foldable (Foldable)
import Control.Applicative (Applicative (..), (<$>))

-- | The type of automorphisms
type Aut a = Iso' a a

data Row3 a =
  Row3 { _left :: a
       , _center :: a
       , _right :: a
       } deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Row3 where
  pure v = Row3 v v v
  Row3 f g h <*> Row3 a b c = Row3 (f a) (g b) (h c)

makeLenses ''Row3

instance Reversing (Row3 a) where
  reversing (Row3 l c r) = Row3 r c l

data Col3 a =
  Col3 { _top :: a
       , _middle :: a
       , _bottom :: a
       } deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Col3 where
  pure v = Col3 v v v
  Col3 f g h <*> Col3 a b c = Col3 (f a) (g b) (h c)

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
          } deriving (Show, Eq, Functor, Foldable, Traversable)

instance  Applicative Side3X3 where
  pure v = Side3X3 v v v v v v v v v
  Side3X3 f1 f2 f3 f4 f5 f6 f7 f8 f9 <*> Side3X3 v1 v2 v3 v4 v5 v6 v7 v8 v9 =
    Side3X3 (f1 v1) (f2 v2) (f3 v3) (f4 v4) (f5 v5) (f6 v6) (f7 v7) (f8 v8) (f9 v9)

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

sideCorners :: Traversal' (Side3X3 a) a
sideCorners f (Side3X3 tl tc tr ml mc mr bl bc br) =
  (\tl' tr' bl' br' -> Side3X3 tl' tc tr' ml mc mr bl' bc br')
  <$> f tl <*> f tr <*> f bl <*> f br

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
       } deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Cube where
  pure v = Cube v v v v v v
  Cube ff fb fl fr fu fd <*> Cube vf vb vl vr vu vd =
    Cube (ff vf) (fb vb) (fl vl) (fr vr) (fu vu) (fd vd)

rotRight' :: Cube a -> Cube a
rotRight' (Cube f b l r u d) = Cube l r b f u d

rotLeft' :: Cube a -> Cube a
rotLeft' (Cube f b l r u d) = Cube r l f b u d

rotateRight' :: Aut (Cube a)
rotateRight' = iso rotRight' rotLeft'

_rotateLeft' :: Aut (Cube a)
_rotateLeft' = from rotateRight'

rotDown' :: Reversing a => Cube a -> Cube a
rotDown' (Cube f b l r u d) = Cube u (reversing d) l r (reversing b) f

rotUp' :: Reversing a => Cube a -> Cube a
rotUp' (Cube f b l r u d) = Cube d (reversing u) l r f (reversing b)

rotateDown' :: Reversing a => Aut (Cube a)
rotateDown' = iso rotDown' rotUp'

_rotateUp' :: Reversing a => Aut (Cube a)
_rotateUp' = from rotateDown'

makeLenses ''Cube

newtype Cube3X3 a =
  Cube3X3 { _cube :: Cube (Side3X3 a)
          } deriving (Show, Eq, Functor)

instance Applicative Cube3X3 where
  pure = Cube3X3 . pure . pure
  Cube3X3 f <*> Cube3X3 v = Cube3X3 ((<*>) <$> f <*> v)

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

rotateDown :: Aut (Cube3X3 a)
rotateDown =
  cong cube $ rotateDown'
            . cong leftSide rotateCW
            . cong rightSide rotateCCW

rotateUp :: Aut (Cube3X3 a)
rotateUp = from rotateDown

data Vec4 a = Vec4 a a a a deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Vec4 where
  pure v = Vec4 v v v v
  Vec4 f1 f2 f3 f4 <*> Vec4 v1 v2 v3 v4 =
    Vec4 (f1 v1) (f2 v2) (f3 v3) (f4 v4)

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
    getter (Cube f b l r _u _d) =
      fmap (view rowLens) (Vec4 f r b l)
    setter (Cube f b l r u d) (Vec4 f' r' b' l') =
      Cube (set rowLens f' f) (set rowLens b' b)
           (set rowLens l' l) (set rowLens r' r) u d

upRows :: RowsLens a
upRows = horizontalRows topRow

middleRows :: RowsLens a
middleRows = horizontalRows middleRow

downRows :: RowsLens a
downRows = horizontalRows bottomRow

moveU :: Aut (Cube3X3 a)
moveU =
  cong cube $ cong upRows cycleLeft
            . cong upSide rotateCW

moveD :: Aut (Cube3X3 a)
moveD =
  cong cube $ cong downRows cycleRight
            . cong downSide rotateCW

verticalCols :: Lens' (Side3X3 a) (Col3 a) -> ColsLens a
verticalCols colLens = lens getter setter
  where
    getter (Cube f b _l _r u d) =
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
    getter (Cube _f _b l r u d) =
      Vec4 (r ^. colLens) (u ^. rotateCW . colLens)
           (l ^. reversed . colLens) (d ^. rotateCCW . colLens)
    setter (Cube f b l r u d) (Vec4 r' u' l' d') =
      Cube f b (set (reversed . colLens) l' l) (set colLens r' r)
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

doMoves :: [Move] -> Aut (Cube3X3 a)
doMoves [] = iso id id
doMoves (m:ms) = move m . doMoves ms

undoMoves :: [Move] -> Aut (Cube3X3 a)
undoMoves = from . doMoves

upLayer :: Traversal' (Cube3X3 a) a
upLayer f =
  cube $ \cube ->
    (\upSide' upRows' -> cube & upSide .~ upSide' & upRows .~ upRows')
       <$> (traverse f (cube ^. upSide))
       <*> (traverse (traverse f) (cube ^. upRows))

middleLayer :: Traversal' (Cube3X3 a) a
middleLayer = cube.middleRows.traverse.traverse

downLayer :: Traversal' (Cube3X3 a) a
downLayer f =
  cube $ \cube ->
    (\downSide' downRows' -> cube & downSide .~ downSide' & downRows .~ downRows')
       <$> (traverse f (cube ^. downSide))
       <*> (traverse (traverse f) (cube ^. downRows))

centerFields :: Traversal' (Cube3X3 a) a
centerFields = cube.traverse.middleCenter

cornerFields :: Traversal' (Cube3X3 a) a
cornerFields = cube.traverse.sideCorners