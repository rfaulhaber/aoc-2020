import           System.IO
import           Text.Parsec
import           Text.Parsec.String

type Color = String
type Bag = (Color, Int)
type Rule = (Color, [Bag])

parseRules :: String -> [Rule]
parseRules = undefined

word :: Parser String
word = do
  w <- many1 letter
  optional spaces
  optional endOfLine
  return w

main = do
  input <- readFile "./input.txt"
  print (parse (many word) "" "foo bar baz\nquux")
