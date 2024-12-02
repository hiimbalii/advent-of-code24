module Day1B (solve1B) where

import Data.Map qualified as Map
import GHC.Conc (par)
import System.IO

type Input = ([Int], Map.Map Int Int)

type Solution = Int

data Side = LeftSide | RightSide

parseInt :: String -> Int
parseInt s = read s

countOccurrences :: [Int] -> Map.Map Int Int
countOccurrences xs = foldr insertCount Map.empty xs
  where
    insertCount x = Map.insertWith (+) x 1

parseInput :: [String] -> Input
parseInput lines = (getSide LeftSide lines, countOccurrences (getSide RightSide lines))
  where
    getSide side lines = map (\line -> parseInt (takeSide side line)) lines
    takeSide LeftSide line = head $ words line
    takeSide RightSide line = head $ tail $ words line

solve :: Input -> Solution
solve ([], _) = 0
solve (x : xs, rhs) = (x * (Map.findWithDefault 0 x rhs)) + (solve (xs, rhs))

solve1B :: [String] -> Int
solve1B = solve . parseInput
