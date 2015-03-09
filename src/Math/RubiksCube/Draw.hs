{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Math.RubiksCube.Draw
  ( solvedCube3X3
  , drawSide3X3
  , drawFoldMap3X3
  , Offsets (Offsets), offsetX, offsetY
  , drawCube
  , drawMove
  , MovesSettings (MovesSettings), moveSep, showStart, showEnd, offsets
  , drawMoves, drawMovesBackward
  ) where

import Math.RubiksCube.Move (Move (..))
import Math.RubiksCube.GraphicalModel

import Control.Lens hiding ((#))
import Diagrams.Prelude hiding (center)
import Data.List (sortBy, mapAccumL)
import Data.Function (on)
import qualified Diagrams.Prelude as P
import Data.Default.Class

solvedCube3X3 :: Cube3X3 (Colour Double)
solvedCube3X3 = Cube3X3 (Cube f b l r u d)
  where
    f = constSide orange
    b = constSide red
    l = constSide white
    r = constSide yellow
    u = constSide green
    d = constSide blue
    constSide v = Side3X3 v v v v v v v v v

drawSide3X3
  :: Renderable (Path R2) b
  => (Double, Double)
  -> (Double, Double)
  -> Side3X3 (Colour Double)
  -> Diagram b R2
drawSide3X3 dx dy side = mconcat $ do
  (y, row) <- zip [(0 :: Int)..] [bottomRow, middleRow, topRow]
  (x, col) <- zip [(0 :: Int)..] [left, center, right]
  let color = side ^. row.col
  return $ fromVertices [pos x y, pos (x+1) y, pos (x+1) (y+1), pos x (y+1), pos x y]
           # mapLoc closeTrail # trailLike # fc color
  where pos x y = p2 $ unr2 $ fromIntegral x *^ r2 dx ^+^ fromIntegral y *^ r2 dy

drawFoldMap3X3
  :: Renderable (Path R2) b
  => Cube3X3 (Colour Double)
  -> Diagram b R2
drawFoldMap3X3 c' =
  let c = c' ^. cube
      drawSide = drawSide3X3 (1,0) (0,1)
  in hcat $ map P.center
       [ drawSide (c ^. leftSide)
       , drawSide (c ^. upSide) ===
         drawSide (c ^. frontSide) ===
         drawSide (c ^. downSide)
       , drawSide (c ^. rightSide)
       , drawSide (c ^. backSide)
       ]

data Offsets =
  Offsets { _offsetX :: Double
          , _offsetY :: Double
          } deriving (Show, Eq, Read)

makeLenses ''Offsets

instance Default Offsets where
  def = Offsets 0.3 0.35

drawCube
  :: Renderable (Path R2) b
  => Offsets
  -> Cube3X3 (Colour Double)
  -> Diagram b R2
drawCube (Offsets dx dy) c' = position $
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
    drawSide dx' dy' side = drawSide3X3 dx' dy' (c' ^. cube . side)
    f = (p2 (0, 0), drawSide (1,0) (0,1) frontSide)
    b = (p2 (3*dx, 3+3*dy), drawSide (1,0) (0,-1) backSide)
    r = (p2 (3,0), drawSide (dx,dy) (0,1) rightSide)
    l = (p2 (3*dx, 3*dy), drawSide (-dx,-dy) (0,1) leftSide)
    u = (p2 (0,3), drawSide (1,0) (dx,dy) upSide)
    d = (p2 (3*dx, 3*dy), drawSide (1,0) (-dx,-dy) downSide)

moveArrow
  :: Renderable (Path R2) b
  => Bool -> P2 -> P2 -> Diagram b R2
moveArrow rev s e =
  (if rev then arrowBetween' opts e s else arrowBetween' opts s e) # lc red
  where opts = with & shaftStyle %~ lw ultraThick & headLength .~ veryLarge

drawMoveU, drawMoveD, drawMoveL, drawMoveR, drawMoveF
  :: Renderable (Path R2) b
  => Bool -- ^ invert
  -> Offsets
  -> Cube3X3 (Colour Double)
  -> Diagram b R2
drawMoveU rev off c =
  atop (moveArrow rev (p2 (2.8, 2.5)) (p2 (0.2, 2.5)))
       (drawCube off c)
drawMoveD rev (Offsets dx dy) c =
  atop (moveArrow rev (p2 (0.2, 0.5)) (p2 (2.8, 0.5)))
       (drawCube (Offsets dx (-dy)) c)
drawMoveL rev (Offsets dx dy) c =
  atop (moveArrow rev (p2 (0.5, 2.8)) (p2 (0.5, 0.2)))
       (drawCube (Offsets dx dy) c)
drawMoveR rev off c =
  atop (moveArrow rev (p2 (2.5, 0.2)) (p2 (2.5, 2.8)))
       (drawCube off c)
drawMoveF rev off c =
  arr (opts & arrowShaft .~ quarterTurn) (p2 (1.5, 2.6)) (p2 (2.5, 1.3))
  `atop`
  arr (opts & arrowShaft .~ quarterTurn) (p2 (1.5, 0.4)) (p2 (0.5, 1.7))
  `atop`
  drawCube off c
  where
    quarterTurn = arc (0 @@ turn) (1/4 @@ turn) # (if rev then id else reverseTrail)
    opts = with & shaftStyle %~ lw ultraThick & headLength .~ veryLarge
    arr opts' s e = (if rev then arrowBetween' opts' e s else arrowBetween' opts' s e)
                   # lc red

drawMove
  :: Renderable (Path R2) b
  => Move
  -> Offsets
  -> Cube3X3 (Colour Double)
  -> Diagram b R2
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
  MovesSettings { _moveSep :: Double
                , _showStart :: Bool
                , _showEnd :: Bool
                , _offsets :: Offsets
                } deriving (Eq, Show, Read)

makeLenses ''MovesSettings

instance Default MovesSettings where
  def = MovesSettings 1.75 False True def

drawMoves
  :: Renderable (Path R2) b
  => MovesSettings
  -> Cube3X3 (Colour Double)
  -> [Move]
  -> Diagram b R2
drawMoves settings c moves =
  let ((j, c'), ps) = mapAccumL iter (0 :: Int, c) moves
      allCubes = (if settings ^. showStart then ((pos ((-1) :: Int), drawCube off c) :) else id) $
                 (if settings ^. showEnd   then ((pos j, drawCube off c') :) else id) ps
  in position allCubes
  where
    off = settings ^. offsets
    pos i = p2 (fromIntegral i * (3 + settings ^. moveSep), 0)
    iter (i, c') m =
      let c'' = c' ^. move m
      in ((i+1, c''), (pos i, drawMove m off c'))

drawMovesBackward
  :: Renderable (Path R2) b
  => MovesSettings
  -> Cube3X3 (Colour Double)
  -> [Move]
  -> Diagram b R2
drawMovesBackward settings c moves =
  drawMoves settings (go c $ reverse moves) moves
  where
    go c' [] = c'
    go c' (m:ms) = go (c' ^. from (move m)) ms
