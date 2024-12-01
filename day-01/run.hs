import Day1A (solve1A)

readLines :: String -> IO [String]
readLines fileName = do
  content <- readFile fileName
  let allLines = lines content
  return allLines

exampleSolution :: Integer
exampleSolution = 11

solveFile :: (Ord a) => ([String] -> a) -> String -> IO a
solveFile solver fileName = do
  lines <- readLines fileName
  let solution = solver lines
  return solution

main :: IO ()
main = do
  exampleComputed <- solve "example_input"
  if exampleComputed == exampleSolution
    then do
      realSolution <- solve "real_input"
      putStrLn $ "Real solution: " ++ show realSolution
    else
      putStrLn $
        "Example test failed: expected "
          ++ show exampleSolution
          ++ ", but got "
          ++ show exampleComputed
  where
    solve = solveFile solve1A