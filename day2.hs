import Data.List (stripPrefix)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

helper1 :: String -> Int -> Int -> (Int, Int)
helper1 command depth horizontal = case stripPrefix "forward " command of
  Just a -> (depth, horizontal + (read a :: Int))
  Nothing -> case stripPrefix "up" command of
    Just a -> (depth - (read a :: Int), horizontal)
    Nothing -> case stripPrefix "down" command of
      Just a -> (depth + (read a :: Int), horizontal)
      Nothing -> (depth, horizontal)

solution_part1 :: [String] -> (Int, Int) -> (Int, Int)
solution_part1 [] (m, n) = (m, n)
solution_part1 (x : xs) (m, n) = solution_part1 xs (helper1 x m n)

part1 = do
  lines <- readLines "day2_test"
  let (m, n) = solution_part1 lines (0, 0)
  print $ m * n

helper2 :: String -> Int -> Int -> Int -> (Int, Int, Int)
helper2 command depth horizontal aim = case stripPrefix "forward " command of
  Just a -> (depth + aim * (read a :: Int), horizontal + (read a :: Int), aim)
  Nothing -> case stripPrefix "up" command of
    Just a -> (depth, horizontal, aim - (read a :: Int))
    Nothing -> case stripPrefix "down" command of
      Just a -> (depth, horizontal, aim + (read a :: Int))
      Nothing -> (depth, horizontal, aim)

solution_part2 :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
solution_part2 [] (m, n, a) = (m, n, a)
solution_part2 (x : xs) (m, n, a) = solution_part2 xs (helper2 x m n a)

part2 = do
  lines <- readLines "day2_test"
  let (m, n, _) = solution_part2 lines (0, 0, 0)
  print $ m * n
