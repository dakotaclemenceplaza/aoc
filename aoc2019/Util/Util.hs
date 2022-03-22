module Util.Util where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))


parse :: String -> [Int]
parse = fst . head . readP_to_S finalParser
  where finalParser = manyTill (item <|> lastItem) eof
        item = do
          n <- number
          _ <- char ','
          return $ read n
        lastItem = do
          n <- number
          _ <- char '\n' >> eof <|> eof
          return $ read n
        number = positiveNum <|> negativeNum
        positiveNum = many1 . choice $ fmap char "0123456789"
        negativeNum = do
          minus <- char '-'
          n <- positiveNum
          return $ minus:n

replace :: a -> Int -> [a] -> [a]
replace v i l = before ++ [v] ++ tail rest
  where (before, rest) = splitAt i l
