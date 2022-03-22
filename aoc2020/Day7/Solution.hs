import Text.ParserCombinators.ReadP (readP_to_S, char, string, many1, choice, sepBy1, endBy1, eof)
import Control.Applicative ((<|>))
import Data.List (nub)

main = do
  i <- readFile "input.txt"
  print . findAnswer2 ["shiny gold"] . map bags . lines $ i
--  print . findAnswer 0 ["shiny gold"] . map bags . lines $ i

findAnswer2 :: [String] -> [(String, [(Int, String)])] -> Int
findAnswer2 bagsToSearch l = if null newBagsToSearch
                             then 0
                             else sum $ map (\(n, b) -> n + n * findAnswer2 [b] l) newBagsToSearch
  where newBagsToSearch = concat . concat $ map (\b -> map snd $ filter (\(k, _) -> b == k) l) bagsToSearch
                
findAnswer acc bagsToSearch l = if null newBagsToSearch
                                then acc
                                else findAnswer newAcc newBagsToSearch newL
  where newBagsToSearch = f bagsToSearch
        f bags = nub . concat . map (\b -> map fst $ filter (\(_, v) -> b `elem` v) l) $ bags
        newL = filter (\(k, _) -> k `notElem` newBagsToSearch) l
        newAcc = acc + length newBagsToSearch

bags :: String -> (String, [(Int, String)])
bags = fst . head . readP_to_S line
  where {-p = do
          p <- sepBy1 line (char '\n')
          _ <- eof
          pure p-}
        line = do
          b <- bag
          c <- containsBags
          pure $ (b,c)
        bag = do
          a <- many1 letter
          _ <- char ' '
          b <- many1 letter
          _ <- char ' '
          _ <- string "bag" <|> string "bags"
          _ <- char ' ' <|> (char ',' >> char ' ') <|> char '.'
          pure $ a <> " " <> b
        numBag = do
          d <- digit
          _ <- char ' '
          b <- bag
          pure $ (read [d], b)
        containsBags = do
          _ <- string "contain "
          bs <- many1 numBag <|> noBags
          _ <- eof
          pure bs
        noBags = string "no other bags." >> pure []
          
letter = choice . map char $ ['a'..'z']
digit = choice . map char $ ['0'..'9']
