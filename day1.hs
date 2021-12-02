import Control.Monad ()
import System.IO ()

toInt :: [String] -> [Int]
toInt = map (\x -> read x :: Int)

readLines :: FilePath -> IO [Int]
readLines = fmap (toInt . lines) . readFile

solution_part1 :: [Int] -> Int
solution_part1 [] = 0
solution_part1 [x] = 0
solution_part1 (x : y : ys) = (if y > x then 1 else 0) + solution_part1 (y : ys)

part1 = do
  lines <- readLines "day1_test"
  print $ solution_part1 lines

solution_part2 :: [Int] -> Int
solution_part2 [] = 0
solution_part2 [x] = 0
solution_part2 [x, y] = 0
solution_part2 [x, y, z] = 0
solution_part2 (x : y : z : t : ts) =
  (if first < second then 1 else 0) + solution_part2 (y : z : t : ts)
  where
    first = x + y + z
    second = y + z + t

part2 = do
  lines <- readLines "day1_test"
  print $ solution_part2 lines
