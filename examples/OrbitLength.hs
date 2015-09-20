{-# LANGUAGE BangPatterns #-}

-- Execute the following two steps repeatedly:
--
-- 1. Rotate the right side up ("R" in standard notation)
-- 2. Rotate the whole cube clockwise around the vertical axis ("Y" in standard notation)
--
-- Question: When will the cube be in its initial position again?

module Main (main) where

import Control.Lens ((^.))
import Diagrams.RubiksCube.Move
import Diagrams.RubiksCube.Model

step :: RubiksCube a -> RubiksCube a
step c = c ^. move R . rotateLeft

solvedRubiksCube :: RubiksCube Int
solvedRubiksCube = RubiksCube (Cube f b l r u d)
  where
    f = pure 1
    b = pure 2
    l = pure 3
    r = pure 4
    u = pure 5
    d = pure 6

answer :: Int
answer = go 1 (step solvedRubiksCube)
  where
    go !i c | c == solvedRubiksCube = i
    go !i c | otherwise = go (i+1) (step c)

main :: IO ()
main = print answer