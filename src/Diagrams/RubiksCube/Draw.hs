{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Diagrams.RubiksCube.Draw
  ( RubiksCubeBackend
  , solvedRubiksCube
  , drawSide
  , drawFoldingPattern
  , Offsets (..), offsetX, offsetY
  , drawRubiksCube
  , drawMove
  , MovesSettings (..), moveSep, showStart, showEnd, offsets
  , drawMoves, drawMovesBackward
  ) where

import Diagrams.RubiksCube.Move (Move (..))
import Diagrams.RubiksCube.Model

import Control.Lens hiding ((#))
import Diagrams.Prelude hiding (center, cube)
import Diagrams.TwoD.Arrow (arrowFromLocatedTrail')
import Diagrams.Trail (trailPoints)
import qualified Diagrams.Prelude as P

import Data.Function (on)
import Data.List (sortBy, mapAccumL)
import Data.Typeable (Typeable)

type RubiksCubeBackend n b = (Renderable (Path V2 n) b, TypeableFloat n, N b ~ n, V b ~ V2)

-- > {-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- > import Diagrams.RubiksCube.Draw
-- > solvedCube = drawFoldingPattern solvedRubiksCube

-- | The solved cube.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_solvedCube.svg#diagram=solvedCube&height=150&width=200>>
solvedRubiksCube :: RubiksCube (Colour Double)
solvedRubiksCube = RubiksCube (Cube f b l r u d)
  where
    f = pure orange
    b = pure red
    l = pure white
    r = pure yellow
    u = pure green
    d = pure blue

-- > {-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- > import Diagrams.RubiksCube
-- > drawSideDia =
-- >   let side = drawSide (r2 (1,0)) (r2 (0,1)) (pure yellow & topLeft .~ green)
-- >       dxArrow = arrowBetween (p2 (4,1)) (p2 (5,1))
-- >       dxLabel = position [(p2 (4.5, 0.7), scale 0.4 (text "dx"))]
-- >       dyArrow = arrowBetween (p2 (4,1)) (p2 (4,2))
-- >       dyLabel = position [(p2 (3.65, 1.5), scale 0.4 (text "dy"))]
-- >   in mconcat [side, dxArrow, dxLabel, dyArrow, dyLabel]

-- | Draws one 3x3 side of the cube.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_drawSideDia.svg#diagram=drawSideDia&height=150&width=200>>
drawSide
  :: (RubiksCubeBackend n b, Color c)
  => V2 n -- ^ dx
  -> V2 n -- ^ dy
  -> Side c
  -> Diagram b
drawSide (dx :: V2 n) dy side = mconcat $ do
  (y, row) <- count rows
  let Vec3 l c r = side ^. row
  [drawField 0 y l, drawField 1 y c, drawField 2 y r]
  where
    count = zip [(0 :: Int)..]
    rows = [bottomRow, middleRow, topRow]
    pos :: Int -> Int -> Point V2 n
    pos x y = P $ fromIntegral x *^ dx ^+^ fromIntegral y *^ dy
    drawField
      :: (Renderable (Path V2 n) b, N b ~ n, V b ~ V2, Color c)
      => Int -> Int -> c -> Diagram b
    drawField x y color =
      fromVertices [pos x y, pos (x+1) y, pos (x+1) (y+1), pos x (y+1), pos x y]
        # mapLoc closeTrail
        # trailLike
        # fillColor color
        # lineCap LineCapRound
        # lineJoin LineJoinRound

-- | Draw the folding pattern of the cube. The front side is at the center of
-- the pattern.
drawFoldingPattern
  :: (RubiksCubeBackend n b, Color c)
  => RubiksCube c
  -> Diagram b
drawFoldingPattern c' =
  let c = c' ^. cube
      drawSide' = drawSide (r2 (1,0)) (r2 (0,1))
  in hcat $ map P.center
       [ drawSide' (c ^. leftSide)
       , drawSide' (c ^. upSide) ===
         drawSide' (c ^. frontSide) ===
         drawSide' (c ^. downSide)
       , drawSide' (c ^. rightSide)
       , drawSide' (c ^. backSide)
       ]

-- > {-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- > import Diagrams.RubiksCube
-- > offsetsDia =
-- >   let off = Offsets 2 1
-- >       c = drawRubiksCube off solvedRubiksCube
-- >       oxArrow = arrowBetween (p2 (3,-1)) (p2 (5,-1))
-- >       oxLabel = position [(p2 (4, -1.75), scale 0.8 (text "offX"))]
-- >       oyArrow = arrowBetween (p2 (7,0)) (p2 (7,1))
-- >       oyLabel = position [(p2 (8, 0.5), scale 0.8 (text "offY"))]
-- >       line start end = fromVertices [p2 start, p2 end]
-- >       lines = mconcat
-- >         [ line (3,-1) (3,0)
-- >         , line (5,-1) (5,2)
-- >         , line (3,0) (7,0)
-- >         , line (5,1) (7,1)
-- >         ] # lc lightgray # dashingN [0.01,0.01] 0
-- >   in mconcat [c, oxArrow, oxLabel, oyArrow, oyLabel, lines] # pad 1.1

-- | <<diagrams/src_Diagrams_RubiksCube_Draw_offsetsDia.svg#diagram=offsetsDia&height=200&width=200>>
data Offsets n =
  Offsets { _offsetX :: n
          , _offsetY :: n
          } deriving (Show, Eq, Read)

makeLenses ''Offsets

instance Fractional n => Default (Offsets n) where
  def = Offsets 0.3 0.35

-- | Draw the Rubik's cube in parallel perspective.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_drawCubeExample.svg#diagram=drawCubeExample&height=150&width=150>>
--
-- > {-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- > import Diagrams.RubiksCube
-- > import Control.Lens
-- > drawCubeExample =
-- >   let c = solvedRubiksCube ^. undoMoves [R,U,R',U']
-- >   in drawRubiksCube with c
drawRubiksCube
  :: (RubiksCubeBackend n b, Color c)
  => Offsets n
  -> RubiksCube c
  -> Diagram b
drawRubiksCube (Offsets dx dy) c' = position $
  [ f ] ++
  sides ++
  [ b ]
  where
    sides = map snd $ sortBy (compare `on` fst) $
      [ (-dx, r)
      , (dx, l)
      , (-dy, u)
      , (dy, d)
      ]
    dx' = r2 (1,0)
    dy' = r2 (0,1)
    dz' = r2 (dx,dy)
    drawSide' dx1 dx2 side = drawSide dx1 dx2 (c' ^. cube . side)
    f = (p2 (0, 0), drawSide' dx' dy' frontSide)
    b = (p2 (3*dx, 3+3*dy), drawSide' dx' (-dy') backSide)
    r = (p2 (3,0), drawSide' dz' dy' rightSide)
    l = (p2 (3*dx, 3*dy), drawSide' (-dz') dy' leftSide)
    u = (p2 (0,3), drawSide' dx' dz' upSide)
    d = (p2 (3*dx, 3*dy), drawSide' dx' (-dz') downSide)

moveArrowOptions :: (Num n, RealFloat n, Fractional n, Typeable n) => ArrowOpts n
moveArrowOptions =
  with
    & shaftStyle %~ lw (local 0.35)
    & headLength .~ local 0.6
    & tailLength .~ local 0.6
    & arrowHead  .~ tri
    & arrowTail  .~ lineTail

moveArrow
  :: RubiksCubeBackend n b
  => Bool
  -> [P2 n]
  -> Diagram b
moveArrow rev points =
  lc red $ arrowFromLocatedTrail' moveArrowOptions $ fromVertices $
    if rev then reverse points else points

drawMoveU, drawMoveD, drawMoveL, drawMoveR, drawMoveF, drawMoveB
  :: (RubiksCubeBackend n b, Color c)
  => Bool -- ^ invert
  -> Offsets n
  -> RubiksCube c
  -> Diagram b
drawMoveU rev off c =
  atop (moveArrow rev [p2 (2.8, 2.5), p2 (0.2, 2.5)])
       (drawRubiksCube off c)
drawMoveD rev (Offsets dx dy) c =
  atop (moveArrow rev [p2 (0.2, 0.5), p2 (2.8, 0.5)])
       (drawRubiksCube (Offsets dx (-dy)) c)
drawMoveL rev off c =
  atop (moveArrow rev [p2 (0.5, 2.8), p2 (0.5, 0.2)])
       (drawRubiksCube off c)
drawMoveR rev off c =
  atop (moveArrow rev [p2 (2.5, 0.2), p2 (2.5, 2.8)])
       (drawRubiksCube off c)
drawMoveF True off c =
    arr (p2 (0.5, 1.2)) (p2 (1.3, 2.5))
  `atop`
    arr (p2 (2.5, 1.8)) (p2 (1.7, 0.5))
  `atop`
    drawRubiksCube off c
  where
    arrOpts = moveArrowOptions & arrowShaft .~ quarterTurn' & arrowTail .~ noTail
    quarterTurn' = arc xDir (0.25 @@ turn)
    arr s e = arrowBetween' arrOpts e s # lc red
drawMoveF False off c =
    arr (p2 (1.7, 2.5)) (p2 (2.5, 1.2))
  `atop`
    arr (p2 (1.3, 0.5)) (p2 (0.5, 1.8))
  `atop`
    drawRubiksCube off c
  where
    arrOpts = moveArrowOptions & arrowShaft .~ quarterTurn' & arrowTail .~ noTail
    quarterTurn' = arc xDir (-0.25 @@ turn)
    arr s e = arrowBetween' arrOpts s e # lc red
drawMoveB rev off@(Offsets dx dy) c =
    moveArrow rev (trailPoints arrowTrail)
  `atop`
    drawRubiksCube off c
  where backOff = p2 (3.3 + 3 * dx, 1.2 + 3 * dy)
        arrowOffsets = [(0 ^& 2.1), ((-2.1) ^& 0)]
        arrowTrail = P.at (fromOffsets arrowOffsets) backOff

-- | Draw the Rubik's cube in parallel perspective with an arrow indicating the
-- next move. If the the bottom layer is moved, the cube will be shown from below.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_drawMoveExample.svg#diagram=drawMoveExample&height=150&width=150>>
--
-- > {-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- > import Diagrams.RubiksCube
-- > import Control.Lens
-- > drawMoveExample =
-- >   let c = solvedRubiksCube ^. undoMoves [L,U,L',U']
-- >   in drawMove L with c
drawMove
  :: (RubiksCubeBackend n b, Color c)
  => Move
  -> Offsets n
  -> RubiksCube c
  -> Diagram b
drawMove U  = drawMoveU False
drawMove U' = drawMoveU True
drawMove D  = drawMoveD False
drawMove D' = drawMoveD True
drawMove L  = drawMoveL False
drawMove L' = drawMoveL True
drawMove R  = drawMoveR False
drawMove R' = drawMoveR True
drawMove F  = drawMoveF False
drawMove F' = drawMoveF True
drawMove B  = drawMoveB False
drawMove B' = drawMoveB True

data MovesSettings n =
  MovesSettings { _moveSep :: n -- ^ space between cubes
                , _showStart :: Bool -- ^ show the start configuration?
                , _showEnd :: Bool -- ^ show the end configuration?
                , _offsets :: Offsets n
                } deriving (Eq, Show, Read)

makeLenses ''MovesSettings

instance Fractional n => Default (MovesSettings n) where
  def = MovesSettings 1.75 False True def

-- | Draws a sequence of moves.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_drawMovesExample.svg#diagram=drawMovesExample&height=100&width=600>>
--
-- > {-# LANGUAGE FlexibleContexts, TypeFamilies #-}
-- > import Diagrams.RubiksCube
-- > import Control.Lens
-- > drawMovesExample =
-- >   let moves = [B, R, F', R', D', F, F]
-- >       startPos = solvedRubiksCube ^. undoMoves moves
-- >       settings = with & showStart .~ True
-- >   in drawMoves settings startPos moves
drawMoves
  :: (RubiksCubeBackend n b, Color c)
  => MovesSettings n
  -> RubiksCube c -- ^ the start configuration
  -> [Move]
  -> Diagram b
drawMoves settings c moves =
  let ((j, c'), ps) = mapAccumL iter (0 :: Int, c) moves
      allCubes = (if settings ^. showStart then ((pos ((-1) :: Int), drawRubiksCube off c) :) else id) $
                 (if settings ^. showEnd   then ((pos j, drawRubiksCube off c') :) else id) ps
  in position allCubes
  where
    off = settings ^. offsets
    pos i = p2 (fromIntegral i * (3 + settings ^. moveSep), 0)
    iter (i, c') m =
      let c'' = c' ^. move m
      in ((i+1, c''), (pos i, drawMove m off c'))

-- | Like 'drawMoves', but takes the end configuration instead of the start
-- configuration. The previous example can be simplified with this:
--
-- > import Diagrams.RubiksCube
-- > import Control.Lens
-- > drawMovesExample' =
-- >   let moves = [B, R, F', R', D', F, F]
-- >       endPos = solvedRubiksCube
-- >       settings = with & showStart .~ True
-- >   in drawMovesBackward settings endPos moves
drawMovesBackward
  :: (RubiksCubeBackend n b, Color c)
  => MovesSettings n
  -> RubiksCube c -- ^ the end configuration
  -> [Move]
  -> Diagram b
drawMovesBackward settings c moves =
  drawMoves settings (c ^. undoMoves moves) moves
