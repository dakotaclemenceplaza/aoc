import Text.ParserCombinators.ReadP (readP_to_S, string, char, many1, choice, sepBy1, endBy1, eof)
import Control.Applicative ((<|>))
import Data.List (intercalate)

main = do
  input <- readFile "input.txt"
  print . length . filter f . fmap pass . passes $ input

f p = all (`elem` map fst p) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] && all f p
  where f ("byr", v) = let num = read v in length v == 4 && num >= 1920 && num <= 2002
        f ("iyr", v) = let num = read v in length v == 4 && num >= 2010 && num <= 2020
        f ("eyr", v) = let num = read v in length v == 4 && num >= 2020 && num <= 2030
        f ("hgt", v) = case (reverse . take 2 . reverse $ v, read . reverse . drop 2. reverse $ v) of
          ("cm", n) -> n >= 150 && n <= 193
          ("in", n) -> n >= 59 && n <= 76
          _ -> False
        f ("hcl", v) = head v == '#' && length v == 7 && all (\s -> s `elem` ['0'..'9'] || s `elem` ['a'..'f']) (tail v)
        f ("ecl", v) = v `elem`  ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        f ("pid", v) = length v == 9 && all (`elem` ['0'..'9']) v
        f ("cid", _) = True
        
pass = fst . head . readP_to_S p
  where p = do
          p <- sepBy1 keys (char ' ')
          _ <- eof
          pure p
        keys = choice $ map f ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]
        f k = do
          key <- string k
          _ <- char ':'
          val <- many1 $ digit <|> letter <|> char '#'
          pure (key, val)
          
passes = fst . head . readP_to_S p
  where p = do
          p <- sepBy1 pass (char '\n')
          _ <- eof
          pure p
        pass = fmap (intercalate " " ) $ endBy1 line (char '\n')
        line = many1 $ letter <|> digit <|> char ' ' <|> char ':' <|> char '#'

letter = choice . map char $ ['a'..'z']
digit = choice . map char $ ['0'..'9']
