module Day1.Solution where

main :: IO ()
main = readFile "input.txt" >>= print . calculate . fmap read . lines

calculateModule m = if result <= 0 then 0 else result + calculateModule result
  where result = m `div` 3 - 2

calculate = sum . map calculateModule

-- 3408471, 5109803
