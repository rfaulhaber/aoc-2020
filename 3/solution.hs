import           System.IO
import           Data.List
import           Text.Printf
import           Debug.Trace

coordinates :: Int -> Int -> (Int, Int) -> [(Int, Int)]
coordinates r d start = iterate (\(x, y) -> (x + d, y + r)) start

slope :: String -> Int -> Char
slope input y = input !! (y `mod` (length input))

trees :: [String] -> Int -> Int -> (Int, Int) -> Int
trees input r d start =
  length . filter (== '#') . map (\(x, y) -> slope (input !! x) y ) $
    tail (take inputMax (coordinates r d start))
  where inputMax = length input

main = do
  input <- (fmap lines . readFile) "./input.txt"
  let part1 = trees input 3 1 (0, 0)
  printf "Part 1: %s" (show part1)
