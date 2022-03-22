import Text.ParserCombinators.ReadP (char, many1, eof, readP_to_S)
import Control.Applicative ((<|>), empty)
import Control.Arrow ((&&&))

main = do
  input <- readFile "input.txt"
  let parsed = fmap (fst . head . readP_to_S policy) . lines $ input
  print $ go parsed
  print $ go2 parsed
  
go = length . filter helper
  where helper ([l], pass, x, y) = uncurry (&&) . ((>=x) &&& (<=y)) . length . filter (==l) $ pass

go2 = length . filter helper
  where helper ([l], pass, x, y) | pass !! (x-1) == l && pass !! (y-1) /= l = True
        helper ([l], pass, x, y) | pass !! (y-1) == l && pass !! (x-1) /= l = True
        helper _ = False
        
digit = foldl (<|>) empty . map char $ "0123456789"
letter = foldl (<|>) empty . map char $ ['a'..'z']

policy = do
  x <- many1 digit
  _ <- char '-'
  y <- many1 digit
  _ <- char ' '
  l <- letter
  _ <- char ':' >> char ' '
  pass <- many1 letter
  _ <- eof
  return ([l], pass, read x, read y)
