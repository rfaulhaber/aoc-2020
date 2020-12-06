import           System.IO
import           Data.List
import           Text.Printf
import           Debug.Trace

slopeSize :: String -> Int
slopeSize = length . head . lines

isTree :: Char -> Bool
isTree c = c == '#'

tt :: (a, a) -> a
tt (_, t) = t

countTrees :: String -> Int -> Int -> Int
countTrees input size offset | offset `mod` size == 0 =
  if isTree (input !! offset) then 1 else 0
countTrees input size offset = if isTree (input !! 3)
  then 1 + (countTrees nextInput size nextOffset)
  else (countTrees nextInput size nextOffset)
 where
  nextInput = (tt . (splitAt (offset + 1))) input
  nextOffset =
    trace ("nextOffset " ++ show (offset + size + 3)) (offset + size + 3)

main = do
  handle   <- openFile "./input.txt" ReadMode
  contents <- hGetContents handle
  let size  = slopeSize contents
  let geo = tt . splitAt size . filter (\c -> c /= '\n') $ contents
  let part1 = countTrees geo size 3
  printf "Part 1: %d" part1
  hClose handle
