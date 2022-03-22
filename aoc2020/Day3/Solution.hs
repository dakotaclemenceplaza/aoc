import Data.List (transpose)

main = do
  input <- readFile "input.txt"
  print . go . lines $ input

go l = a * b
  where len = length l
        slopes = ([1, 3, 5, 7], 2)
        makeMapAndFold r = (f r, take len . map (take $ len * r) . map concat . transpose . repeat $ l)
        a = product . map (snd . uncurry (flip foldl (0, 0)) . makeMapAndFold) . fst $ slopes
        b = snd . uncurry (flip foldl (0, 0)) . fmap transpose . makeMapAndFold . snd $ slopes
        f r (x, y) ll | x < length ll && ll !! x == '#' = (x + r, y + 1)
        f r (x, y) _ = (x + r, y)

