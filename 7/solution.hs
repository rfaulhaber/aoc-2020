import           System.IO
import qualified Data.Map.Strict               as Map

type Color = String
type Bag = (Color, Int)
type Rule = (Color, [Bag])

-- parsing inspiration came from:
-- https://www.michaelcw.com/programming/2020/12/11/aoc-2020-d7.html
-- thank you!

parseLine :: String -> Rule
parseLine l =
  let w     = words l
      color = unwords (take 2 w)
      bags  = parseBag (drop 4 w)
  in  if length w == 7 then (color, []) else (color, bags)

parseBag :: [String] -> [Bag]
parseBag [] = []
parseBag w =
  let c     = read (head w)
      color = (unwords . take 2 . tail) $ w
  in  (color, c) : parseBag (drop 4 w)

tupleHead :: (a, b) -> a
tupleHead (a, _) = a

-- I don't know how to solve this in Haskell yet!


main = do
  input <- readFile "./practice.txt"
  print ((map parseLine . lines) $ input)
