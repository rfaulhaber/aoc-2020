import System.IO
import Data.String

split :: String -> String -> [String]
split s [] = [""]
split s (h:t) | [h] == s = "" : rest
              | otherwise = (h : head rest) : tail rest
  where rest = split s t

solve = do
    handle <- openFile "./input.txt" ReadMode  
    contents <- hGetContents handle  
    print (split "," "foo,bar")
    hClose handle

main = solve
