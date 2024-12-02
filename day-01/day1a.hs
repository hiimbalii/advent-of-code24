module Day1A (solve1A) where

import Data.List
import System.IO

type Input = ([Integer], [Integer])

type Solution = Integer

data Side = LeftSide | RightSide

parseInt :: String -> Integer
parseInt s = read s

parseInput :: [String] -> Input
parseInput lines = (getSide LeftSide lines, getSide RightSide lines)
  where
    getSide side lines = sort $ map (\line -> parseInt (takeSide side line)) lines
    takeSide LeftSide line = head $ words line
    takeSide RightSide line = head $ tail $ words line

solve :: Input -> Solution
solve ([], []) = 0
solve (x : xs, y : ys) = (abs (x - y)) + (solve (xs, ys))

solve1A :: [String] -> Integer
solve1A = solve . parseInput
