import Text.ParserCombinators.ReadP (readP_to_S, char, many1, choice, sepBy1, endBy1, eof)
import Control.Applicative ((<|>))
import Data.List (intercalate, nub, intersect)

main = do
  i <- readFile "input.txt"
  print . sum . fmap (length . nub) . groups $ i
  print . sum . fmap (length . nub . foldl1 intersect) . groups2 $ i
  
groups = fst . head . readP_to_S p
  where p = do
          p <- sepBy1 group (char '\n')
          _ <- eof
          pure p
        group = fmap concat $ endBy1 line (char '\n')
        line = many1 letter

groups2 = fst . head . readP_to_S p
  where p = do
          p <- sepBy1 group (char '\n')
          _ <- eof
          pure p
        group = endBy1 line (char '\n')
        line = many1 letter

letter = choice . map char $ ['a'..'z']
