import           System.IO
import           Data.List
import           Text.Printf

coordinates :: Int -> Int -> (Int, Int) -> [(Int, Int)]
coordinates r d start = iterate (\(x, y) -> (x + d, y + r)) start

slope :: String -> Int -> Char
slope input y = input !! (y `mod` (length input))

trees :: [String] -> Int -> Int -> (Int, Int) -> Int
trees input r d start =
  length . filter (== '#') . map (\(x, y) -> slope (input !! x) y ) $
    tail (take inputMax (coordinates r d start))
  where inputMax = (length input) `div` d

main = do
  input <- (fmap lines . readFile) "./input.txt"
  let part1 = trees input 3 1 (0, 0)
  let part2 = product . map(\(r, d) -> trees input r d (0, 0)) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  printf "Part 1: %s\n" (show part1)
  printf "Part 2: %s" (show part2)
