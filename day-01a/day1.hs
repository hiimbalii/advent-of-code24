import Data.List
import Data.Set qualified as Set
import GHC.Conc (par)
import System.IO

exampleSolution :: Integer
exampleSolution = 11

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

-- Handling IO etc

readLines :: String -> IO [String]
readLines fileName = do
  content <- readFile fileName
  let allLines = lines content
  return allLines

solveFile :: String -> IO Solution
solveFile fileName = do
  exampleLines <- readLines fileName
  let solution = solve (parseInput exampleLines)
  return solution

main :: IO ()
main = do
  exampleComputed <- solveFile "example_input"
  if exampleComputed == exampleSolution
    then do
      realSolution <- solveFile "real_input"
      putStrLn $ "Real solution: " ++ show realSolution
    else
      putStrLn $
        "Example test failed: expected "
          ++ show exampleSolution
          ++ ", but got "
          ++ show exampleComputed