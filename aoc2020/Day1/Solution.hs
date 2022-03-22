import Control.Arrow ((&&&))
import Data.List (find)

main = do
  i <- readFile "input.txt"
  let nums = map read . lines $ i
  print (go nums, go2 nums)

go [] = Nothing
go (x:xs) = case find ((==2020) . fst) . map ((x+) &&& (x*)) $ xs of
  Just (_, m) -> Just m
  Nothing -> go xs

go2 l = head [x * y * z | x <- l, y <- l, z <- l, x + y + z == 2020]  
