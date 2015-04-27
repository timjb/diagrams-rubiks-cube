{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

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
import Data.List (sortBy, mapAccumL)
import Data.Function (on)
import qualified Diagrams.Prelude as P

type RubiksCubeBackend b = (Renderable (Path V2 Double) b, N b ~ Double, V b ~ V2)

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
  :: RubiksCubeBackend b
  => V2 Double -- ^ dx
  -> V2 Double -- ^ dy
  -> Side (Colour Double)
  -> Diagram b
drawSide dx dy side = mconcat $ do
  (y, row) <- count rows
  let Vec3 l c r = side ^. row
  [drawField 0 y l, drawField 1 y c, drawField 2 y r]
  where
    count = zip [(0 :: Int)..]
    rows = [bottomRow, middleRow, topRow]
    pos :: Int -> Int -> Point V2 Double
    pos x y = P $ fromIntegral x *^ dx ^+^ fromIntegral y *^ dy
    drawField
      :: (Renderable (Path V2 Double) b, N b ~ Double, V b ~ V2)
      => Int -> Int -> Colour Double -> Diagram b
    drawField x y color =
      fromVertices [pos x y, pos (x+1) y, pos (x+1) (y+1), pos x (y+1), pos x y]
        # mapLoc closeTrail # trailLike # fc color

-- | Draw the folding pattern of the cube. The front side is at the center of
-- the pattern.
drawFoldingPattern
  :: RubiksCubeBackend b
  => RubiksCube (Colour Double)
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
data Offsets =
  Offsets { _offsetX :: Double
          , _offsetY :: Double
          } deriving (Show, Eq, Read)

makeLenses ''Offsets

instance Default Offsets where
  def = Offsets 0.3 0.35

-- | Draw the Rubik's cube in parallel perspective.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_drawCubeExample.svg#diagram=drawCubeExample&height=150&width=150>>
--
-- > import Diagrams.RubiksCube
-- > import Control.Lens
-- > drawCubeExample =
-- >   let c = solvedRubiksCube ^. undoMoves [R,U,R',U']
-- >   in drawRubiksCube with c
drawRubiksCube
  :: RubiksCubeBackend b
  => Offsets
  -> RubiksCube (Colour Double)
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

moveArrow
  :: RubiksCubeBackend b
  => Bool -> P2 Double -> P2 Double -> Diagram b
moveArrow rev s e =
  (if rev then arrowBetween' opts e s else arrowBetween' opts s e) # lc red
  where opts = with & shaftStyle %~ lw ultraThick & headLength .~ veryLarge

drawMoveU, drawMoveD, drawMoveL, drawMoveR, drawMoveF
  :: RubiksCubeBackend b
  => Bool -- ^ invert
  -> Offsets
  -> RubiksCube (Colour Double)
  -> Diagram b
drawMoveU rev off c =
  atop (moveArrow rev (p2 (2.8, 2.5)) (p2 (0.2, 2.5)))
       (drawRubiksCube off c)
drawMoveD rev (Offsets dx dy) c =
  atop (moveArrow rev (p2 (0.2, 0.5)) (p2 (2.8, 0.5)))
       (drawRubiksCube (Offsets dx (-dy)) c)
drawMoveL rev (Offsets dx dy) c =
  atop (moveArrow rev (p2 (0.5, 2.8)) (p2 (0.5, 0.2)))
       (drawRubiksCube (Offsets dx dy) c)
drawMoveR rev off c =
  atop (moveArrow rev (p2 (2.5, 0.2)) (p2 (2.5, 2.8)))
       (drawRubiksCube off c)
drawMoveF rev off c =
  arr (opts & arrowShaft .~ quarterTurn') (p2 (1.5, 2.6)) (p2 (2.5, 1.3))
  `atop`
  arr (opts & arrowShaft .~ quarterTurn') (p2 (1.5, 0.4)) (p2 (0.5, 1.7))
  `atop`
  drawRubiksCube off c
  where
    quarterTurn' = arc xDir (1/4 @@ turn) # (if rev then id else reverseTrail)
    opts = with & shaftStyle %~ lw ultraThick & headLength .~ veryLarge
    arr opts' s e = (if rev then arrowBetween' opts' e s else arrowBetween' opts' s e)
                   # lc red

-- | Draw the Rubik's cube in parallel perspective with an arrow indicating the
-- next move. If the the bottom layer is moved, the cube will be shown from below.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_drawMoveExample.svg#diagram=drawMoveExample&height=150&width=150>>
--
-- > import Diagrams.RubiksCube
-- > import Control.Lens
-- > drawMoveExample =
-- >   let c = solvedRubiksCube ^. undoMoves [L,U,L',U']
-- >   in drawMove L with c
drawMove
  :: RubiksCubeBackend b
  => Move
  -> Offsets
  -> RubiksCube (Colour Double)
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
drawMove _  = error "can't draw back moves!"

data MovesSettings =
  MovesSettings { _moveSep :: Double -- ^ space between cubes
                , _showStart :: Bool -- ^ show the start configuration?
                , _showEnd :: Bool -- ^ show the end configuration?
                , _offsets :: Offsets
                } deriving (Eq, Show, Read)

makeLenses ''MovesSettings

instance Default MovesSettings where
  def = MovesSettings 1.75 False True def

-- | Draws a sequence of moves.
--
-- <<diagrams/src_Diagrams_RubiksCube_Draw_drawMovesExample.svg#diagram=drawMovesExample&height=100&width=600>>
--
-- > import Diagrams.RubiksCube
-- > import Control.Lens
-- > drawMovesExample =
-- >   let moves = [R, F', R', D', F, F]
-- >       startPos = solvedRubiksCube ^. undoMoves moves
-- >       settings = with & showStart .~ True
-- >   in drawMoves settings startPos moves
drawMoves
  :: RubiksCubeBackend b
  => MovesSettings
  -> RubiksCube (Colour Double) -- ^ the start configuration
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
-- >   let moves = [R, F', R', D', F, F]
-- >       endPos = solvedRubiksCube
-- >       settings = with & showStart .~ True
-- >   in drawMovesBackward settings endPos moves
drawMovesBackward
  :: RubiksCubeBackend b
  => MovesSettings
  -> RubiksCube (Colour Double) -- ^ the end configuration
  -> [Move]
  -> Diagram b
drawMovesBackward settings c moves =
  drawMoves settings (c ^. undoMoves moves) moves
