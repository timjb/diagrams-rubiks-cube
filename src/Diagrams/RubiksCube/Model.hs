{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Diagrams.RubiksCube.Model (
  -- * Constructing cubes
    Side (..)
  , topLeft, topCenter, topRight
  , middleLeft, middleCenter, middleRight
  , bottomLeft, bottomCenter, bottomRight
  , rotateSideCW, rotateSideCCW
  , Cube (..), frontSide, backSide, leftSide, rightSide, upSide, downSide
  , RubiksCube (..), cube
  -- * Selecting rows and columns
  , Vec3 (..)
  , topRow, middleRow, bottomRow
  , leftCol, centerCol, rightCol
  -- * Traversing facets
  -- ** By layer
  , topLayerFacets, middleLayerFacets, bottomLayerFacets
  -- ** By position
  , centerFacets, cornerFacets, edgeFacets
  , sideCorners, sideEdges
  -- ** By rows
  , upRows
  , middleRows
  , downRows
  -- ** By cols
  , leftCols
  , centerCols
  , rightCols
  , frontCols
  , betweenCols
  , backCols
  -- * Rotating the whole cube
  , Aut
  , rotateLeft, rotateRight
  , rotateDown, rotateUp
  , rotateCW, rotateCCW
  -- * Moving layers of the cube
  , move, doMoves, undoMoves
  ) where

import Control.Lens
import Diagrams.RubiksCube.Move (Move (..))
import Data.Foldable (Foldable)
import Control.Applicative (Applicative (..), (<$>))
import Data.Distributive (Distributive (..))
import Data.Functor.Rep (Representable (..), distributeRep, tabulated)

-- | The type of automorphisms
type Aut a = Iso' a a

-- Natural numbers
data N = Z | S N

-- Some type synonyms for natural numbers
type Zero = 'Z
type One = 'S Zero
type Two = 'S One
type Three = 'S Two
type Four = 'S Three

-- | Finite type with n inhabitants
data Fin :: N -> * where
  FinZ :: Fin ('S n)
  FinS :: Fin n -> Fin ('S n)

zero :: Fin ('S n)
zero = FinZ

one :: Fin ('S ('S n))
one = FinS zero

two :: Fin ('S ('S ('S n)))
two = FinS one

three :: Fin ('S ('S ('S ('S n))))
three = FinS two

-- | A list of fixed length 3.
data Vec3 a
  = Vec3 a a a
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Vec3 where
  pure v = Vec3 v v v
  Vec3 f1 f2 f3 <*> Vec3 v1 v2 v3 =
    Vec3 (f1 v1) (f2 v2) (f3 v3)

instance Reversing (Vec3 a) where
  reversing (Vec3 a b c) = Vec3 c b a

instance Distributive Vec3 where
  distribute = distributeRep

instance Representable Vec3 where
  type Rep Vec3 = Fin Three
  tabulate f = Vec3 (f zero) (f one) (f two)
  index (Vec3 a b c) fin =
    case fin of
      FinZ -> a
      FinS FinZ -> b
      FinS (FinS FinZ) -> c
      _ -> error "index@Vec3: cannot happen"
      
instance Field1 (Vec3 a) (Vec3 a) a a where
  _1 f (Vec3 a b c) = (\a' -> Vec3 a' b c) <$> f a
      
instance Field2 (Vec3 a) (Vec3 a) a a where
  _2 f (Vec3 a b c) = (\b' -> Vec3 a b' c) <$> f b
      
instance Field3 (Vec3 a) (Vec3 a) a a where
  _3 f (Vec3 a b c) = (\c' -> Vec3 a b c') <$> f c

-- | A variant of 'inside' that works for
insideRep
  :: Representable g
  => Lens s t a b
  -> Lens (g s) (g t) (g a) (g b)
insideRep l = from tabulated . inside l . tabulated

-- | A list of fixed length 4.
data Vec4 a = Vec4 a a a a deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Vec4 where
  pure v = Vec4 v v v v
  Vec4 f1 f2 f3 f4 <*> Vec4 v1 v2 v3 v4 =
    Vec4 (f1 v1) (f2 v2) (f3 v3) (f4 v4)

instance Distributive Vec4 where
  distribute = distributeRep

instance Representable Vec4 where
  type Rep Vec4 = Fin Four
  tabulate f = Vec4 (f zero) (f one) (f two) (f three)
  index (Vec4 a b c d) fin =
    case fin of
      FinZ -> a
      FinS FinZ -> b
      FinS (FinS FinZ) -> c
      FinS (FinS (FinS FinZ)) -> d
      _ -> error "index@Vec4: cannot happen"

cycRight :: Vec4 a -> Vec4 a
cycRight (Vec4 a b c d) = Vec4 d a b c

cycLeft :: Vec4 a -> Vec4 a
cycLeft (Vec4 a b c d) = Vec4 b c d a

cycleLeft :: Aut (Vec4 a)
cycleLeft = iso cycLeft cycRight

cycleRight :: Aut (Vec4 a)
cycleRight = iso cycRight cycLeft

-- | One side of the Rubik's Cube with 3*3 facets.
data Side a =
  Side { _topLeft :: a
          , _topCenter :: a
          , _topRight :: a
          , _middleLeft :: a
          , _middleCenter :: a
          , _middleRight :: a
          , _bottomLeft :: a
          , _bottomCenter :: a
          , _bottomRight :: a
          } deriving (Show, Eq, Functor, Foldable, Traversable)

instance  Applicative Side where
  pure v = Side v v v v v v v v v
  Side f1 f2 f3 f4 f5 f6 f7 f8 f9 <*> Side v1 v2 v3 v4 v5 v6 v7 v8 v9 =
    Side (f1 v1) (f2 v2) (f3 v3) (f4 v4) (f5 v5) (f6 v6) (f7 v7) (f8 v8) (f9 v9)

makeLenses ''Side

instance Reversing (Side a) where
  reversing (Side tl tc tr ml mc mr bl bc br) =
             Side br bc bl mr mc ml tr tc tl

rotCW :: Side a -> Side a
rotCW (Side tl tc tr ml mc mr bl bc br) =
       Side bl ml tl bc mc tc br mr tr

rotCCW :: Side a -> Side a
rotCCW (Side tl tc tr ml mc mr bl bc br) =
        Side tr mr br tc mc bc tl ml bl

-- | Rotate the side clockwise.
rotateSideCW :: Aut (Side a)
rotateSideCW = iso rotCW rotCCW

-- | Rotate the side counter-clockwise.
rotateSideCCW :: Aut (Side a)
rotateSideCCW = iso rotCCW rotCW

-- | The top three facets (from left to right).
topRow :: Lens' (Side a) (Vec3 a)
topRow = lens getter setter
  where
    getter (Side tl tc tr _ _ _ _ _ _) = Vec3 tl tc tr
    setter (Side _ _ _ ml mc mr bl bc br) (Vec3 tl tc tr) =
      Side tl tc tr ml mc mr bl bc br

-- | The middle three facets (from left to right).
middleRow :: Lens' (Side a) (Vec3 a)
middleRow = lens getter setter
  where
    getter (Side _ _ _ ml mc mr _ _ _) = Vec3 ml mc mr
    setter (Side tl tc tr _ _ _ bl bc br) (Vec3 ml mc mr) =
      Side tl tc tr ml mc mr bl bc br

-- | The bottom three facets (from left to right).
bottomRow :: Lens' (Side a) (Vec3 a)
bottomRow = lens getter setter
  where
    getter (Side _ _ _ _ _ _ bl bc br) = Vec3 bl bc br
    setter (Side tl tc tr ml mc mr _ _ _) (Vec3 bl bc br) =
      Side tl tc tr ml mc mr bl bc br

-- | The left column (from top to down).
leftCol :: Lens' (Side a) (Vec3 a)
leftCol = lens getter setter
  where
    getter (Side tl _ _ ml _ _ bl _ _) = Vec3 tl ml bl
    setter (Side _ tc tr _ mc mr _ bc br) (Vec3 tl ml bl) =
      Side tl tc tr ml mc mr bl bc br

-- | The center column (from top to down).
centerCol :: Lens' (Side a) (Vec3 a)
centerCol = lens getter setter
  where
    getter (Side _ tc _ _ mc _ _ bc _) = Vec3 tc mc bc
    setter (Side tl _ tr ml _ mr bl _ br) (Vec3 tc mc bc) =
      Side tl tc tr ml mc mr bl bc br

-- | The right column (from top to down).
rightCol :: Lens' (Side a) (Vec3 a)
rightCol = lens getter setter
  where
    getter (Side _ _ tr _ _ mr _ _ br) = Vec3 tr mr br
    setter (Side tl tc _ ml mc _ bl bc _) (Vec3 tr mr br) =
      Side tl tc tr ml mc mr bl bc br

-- | The four corners of a side.
sideCorners :: Traversal' (Side a) a
sideCorners f (Side tl tc tr ml mc mr bl bc br) =
  (\tl' tr' bl' br' -> Side tl' tc tr' ml mc mr bl' bc br')
  <$> f tl <*> f tr <*> f bl <*> f br

-- | The four edges of a side.
sideEdges :: Traversal' (Side a) a
sideEdges f (Side tl tc tr ml mc mr bl bc br) =
  (\tc' ml' mr' bc' -> Side tl tc' tr ml' mc mr' bl bc' br)
  <$> f tc <*> f ml <*> f mr <*> f bc

-- | A cube with six sides.
--
-- @
--      +---+
--      | u |
--  +---+---+---+---+
--  | l | f | r | b |
--  +---+---+---+---+
--      | d |
--      +---+
-- @
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

-- | A normal Rubik's cube with 6 sides with 9 facets each.
newtype RubiksCube a =
  RubiksCube { _cube :: Cube (Side a)
             } deriving (Show, Eq, Functor)

instance Applicative RubiksCube where
  pure = RubiksCube . pure . pure
  RubiksCube f <*> RubiksCube v = RubiksCube ((<*>) <$> f <*> v)

makeLenses ''RubiksCube

cong :: Traversal' s a -> Aut a -> Aut s
cong t i = withIso i $ \f g -> iso (over t f) (over t g)

-- | Rotate the whole Rubik's Cube such that the front side becomes the new
-- right side and the top and bottom sides stay fixed.
rotateRight :: Aut (RubiksCube a)
rotateRight =
  cong cube $ rotateRight'
            . cong upSide rotateSideCCW
            . cong downSide rotateSideCW

-- | Rotate the whole Rubik's Cube such that the front side becomes the new
-- left side and the top and bottom sides stay fixed.
rotateLeft :: Aut (RubiksCube a)
rotateLeft = from rotateRight

-- | Rotate the whole Rubik's Cube such that the front side becomes the new
-- bottom side and the left and right sides stay fixed.
rotateDown :: Aut (RubiksCube a)
rotateDown =
  cong cube $ rotateDown'
            . cong leftSide rotateSideCW
            . cong rightSide rotateSideCCW

-- | Rotate the whole Rubik's Cube such that the front side becomes the new
-- top side and the left and right sides stay fixed.
rotateUp :: Aut (RubiksCube a)
rotateUp = from rotateDown

-- | Rotate the whole Rubik's Cube such that the top side becomes the new
-- right side and the front and back sides stay fixed.
rotateCW :: Aut (RubiksCube a)
rotateCW = rotateUp . rotateLeft . rotateDown

-- | Rotate the whole Rubik's Cube such that the top side becomes the new
-- left side and the front and back sides stay fixed.
rotateCCW :: Aut (RubiksCube a)
rotateCCW = from rotateCW

type RowsLens a = Lens' (Cube (Side a)) (Vec4 (Vec3 a))
type ColsLens a = Lens' (Cube (Side a)) (Vec4 (Vec3 a))

horizontalSides :: Lens' (Cube a) (Vec4 a)
horizontalSides = lens getter setter
  where
    getter (Cube f b l r _u _d) = Vec4 f r b l
    setter (Cube _f _b _l _r u d) (Vec4 f' r' b' l') =
            Cube f' b' l' r' u d

horizontalRows :: Lens' (Side a) (Vec3 a) -> RowsLens a
horizontalRows rowLens = horizontalSides . insideRep rowLens

upRows :: RowsLens a
upRows = horizontalRows topRow

middleRows :: RowsLens a
middleRows = horizontalRows middleRow

downRows :: RowsLens a
downRows = horizontalRows bottomRow

moveU :: Aut (RubiksCube a)
moveU =
  cong cube $ cong upRows cycleLeft
            . cong upSide rotateSideCW

moveD :: Aut (RubiksCube a)
moveD =
  cong cube $ cong downRows cycleRight
            . cong downSide rotateSideCW

verticalCols :: Lens' (Side a) (Vec3 a) -> ColsLens a
verticalCols colLens = lens getter setter
  where
    getter (Cube f b _l _r u d) =
      Vec4 (f ^. colLens) (u ^. colLens) (b ^. reversed . colLens) (d ^. colLens)
    setter (Cube f b l r u d) (Vec4 f' u' b' d') =
      Cube (set colLens f' f) (set (reversed . colLens) b' b) l r
           (set colLens u' u) (set colLens d' d)

leftCols :: ColsLens a
leftCols = verticalCols leftCol

centerCols :: ColsLens a
centerCols = verticalCols centerCol

rightCols :: ColsLens a
rightCols = verticalCols rightCol

moveL :: Aut (RubiksCube a)
moveL =
  cong cube $ cong leftCols cycleLeft
            . cong leftSide rotateSideCW

moveR :: Aut (RubiksCube a)
moveR =
  cong cube $ cong rightCols cycleRight
           . cong rightSide rotateSideCW

ringCols :: Lens' (Side a) (Vec3 a) -> ColsLens a
ringCols colLens = lens getter setter
  where
    getter (Cube _f _b l r u d) =
      Vec4 (r ^. colLens) (u ^. rotateSideCW . colLens)
           (l ^. reversed . colLens) (d ^. rotateSideCCW . colLens)
    setter (Cube f b l r u d) (Vec4 r' u' l' d') =
      Cube f b (set (reversed . colLens) l' l) (set colLens r' r)
               (set (rotateSideCW . colLens) u' u) (set (rotateSideCCW . colLens) d' d)

frontCols :: ColsLens a
frontCols = ringCols leftCol

betweenCols :: ColsLens a
betweenCols = ringCols centerCol

backCols :: ColsLens a
backCols = ringCols rightCol

moveF :: Aut (RubiksCube a)
moveF =
  cong cube $ cong frontCols cycleLeft
            . cong frontSide rotateSideCW

moveB :: Aut (RubiksCube a)
moveB =
  cong cube $ cong backCols cycleRight
            . cong backSide rotateSideCW

-- | Perform a move.
move :: Move -> Aut (RubiksCube a)
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

-- | Perform a list of moves.
doMoves :: [Move] -> Aut (RubiksCube a)
doMoves [] = iso id id
doMoves (m:ms) = move m . doMoves ms

-- | Undo the actions of a list of moves.
undoMoves :: [Move] -> Aut (RubiksCube a)
undoMoves = from . doMoves

-- | The 21=4*3+9 facets in the top layer.
topLayerFacets :: Traversal' (RubiksCube a) a
topLayerFacets f =
  cube $ \c ->
    (\upSide' upRows' -> c & upSide .~ upSide' & upRows .~ upRows')
       <$> (traverse f (c ^. upSide))
       <*> (traverse (traverse f) (c ^. upRows))

-- | The 12=4*3 facets in the middle layer.
middleLayerFacets :: Traversal' (RubiksCube a) a
middleLayerFacets = cube.middleRows.traverse.traverse

-- | The 21=4*3+9 facets in the bottom layer.
bottomLayerFacets :: Traversal' (RubiksCube a) a
bottomLayerFacets f =
  cube $ \c ->
    (\downSide' downRows' -> c & downSide .~ downSide' & downRows .~ downRows')
       <$> (traverse f (c ^. downSide))
       <*> (traverse (traverse f) (c ^. downRows))

-- | The six facets that are the center of their side.
centerFacets :: Traversal' (RubiksCube a) a
centerFacets = cube.traverse.middleCenter

-- | The 24=6*4=8*3 corner facets.
cornerFacets :: Traversal' (RubiksCube a) a
cornerFacets = cube.traverse.sideCorners

-- | The 24=6*4=12*2 edge facets.
edgeFacets :: Traversal' (RubiksCube a) a
edgeFacets = cube.traverse.sideEdges
