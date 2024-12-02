import Day1A (solve1A)
import Day1B (solve1B)
import Distribution.InstalledPackageInfo (InstalledPackageInfo (exposedModules))

readLines :: String -> IO [String]
readLines fileName = do
  content <- readFile fileName
  let allLines = lines content
  return allLines

exampleSolutionA :: Integer
exampleSolutionA = 11

exampleSolutionB :: Int
exampleSolutionB = 31

solveFile :: (Ord a) => ([String] -> a) -> String -> IO a
solveFile solver fileName = do
  lines <- readLines fileName
  let solution = solver lines
  return solution

main :: IO ()
main = do
  solvePart solve1A exampleSolutionA
  solvePart solve1B exampleSolutionB
  where
    solvePart solver example = do
      exampleComputed <- solveFile solver "example_input"
      if exampleComputed == example
        then do
          realSolution <- solveFile solver "real_input"
          putStrLn $ "Real solution: " ++ show realSolution
        else
          putStrLn $
            "Example test failed: expected "
              ++ show example
              ++ ", but got "
              ++ show exampleComputed
