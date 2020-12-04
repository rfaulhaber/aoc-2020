{-# LANGUAGE NamedFieldPuns #-}
import System.IO
import Data.String
import Text.Printf

data Range = Range {
  low :: Integer,
  high :: Integer
  } deriving (Show)

data Line = Line {
  range :: Range,
  letter :: Char,
  password :: String
  } deriving (Show)

-- thank you stackoverflow
split :: String -> String -> [String]
split s [] = [""]
split s (h:t) | [h] == s = "" : rest
              | otherwise = (h : head rest) : tail rest
  where rest = split s t

charCount :: Char -> String -> Integer
charCount c "" = 0
charCount c (h:t) | c == h = 1 + charCount c t
                  | otherwise = charCount c t

inRange :: Range -> Integer -> Bool
inRange Range {low, high} test = (test >= low) && (test <= high)

parseRange :: String -> Range
parseRange input = do
  let (h:t:_) = ((map (\s -> read s :: Integer)) . (take 2) . (split "-")) $ input
  Range {low = h, high = t}

parseLine :: String -> Line
parseLine input = do
  let (r:c:p:_) = split " " input
  Line {range = (parseRange r), letter = (head c), password = p}

isLineValid :: Line -> Bool
isLineValid Line {range, letter, password} = inRange range (charCount letter password)

countValidLines :: [Line] -> Integer
countValidLines (h:t) = if isLineValid h then 1 + countValidLines t else countValidLines t
countValidLines [] = 0

solve = do
    handle <- openFile "./input.txt" ReadMode  
    contents <- hGetContents handle  
    -- print (split "\n" contents)
    let part1 = ((countValidLines . (map parseLine) . init . (split "\n")) $ contents)
    printf "Part 1: %d" part1
    hClose handle

main = solve
