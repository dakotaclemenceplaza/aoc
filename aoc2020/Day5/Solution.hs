import Data.List (sort)

main = do
  i <- readFile "input.txt"
  let seats = go . lines $ i
  print $ maximum seats
  print . findSeat . sort $ seats
  
go :: [String] -> [Double]
go = map (g . foldl f ((0, 127, True),(0, 7, True)))
  where f ((x, y, _), c) 'F' = ((x, fromIntegral . floor $ (x + y) / 2, True), c)
        f ((x, y, _), c) 'B' = ((fromIntegral . ceiling $ (x + y) / 2, y, False), c)
        f (r, (x, y, _)) 'L' = (r, (x, fromIntegral . floor $ (x + y) / 2, True))
        f (r, (x, y, _)) 'R' = (r, ((fromIntegral. ceiling $ (x + y) / 2), y, False))
        g ((x, _, True), (n, _, True)) = x * 8 + n
        g ((x, _, True), (_, m, False)) = x * 8 + m
        g ((_, y, False), (n, _, True)) = y * 8 + n
        g ((_, y, False), (_, m, False)) = y * 8 + m

findSeat [] = 0
findSeat [_] = 0
findSeat (x:y:_) | y - x == 2 = x + 1
findSeat (_:xs) = findSeat xs
