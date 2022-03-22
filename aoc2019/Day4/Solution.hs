module Day4.Solution where

import Control.Monad (guard)

-- 272091-815432

answer = length $ do
  x1 <- [2..8]
  x2 <- [x1..9]
  x3 <- [x2..9]
  x4 <- [x3..9]
  x5 <- [x4..9]
  x6 <- [x5..9]
  guard $ x1 == x2 && x2 /= x3
    || x2 == x3 && x1 /= x2 && x3 /= x4
    || x3 == x4 && x2 /= x3 && x4 /= x5
    || x4 == x5 && x3 /= x4 && x5 /= x6
    || x5 == x6 && x4 /= x5
  let s = concatMap show [x1, x2, x3, x4, x5, x6]
      n = read s
  guard $ n > 272091 && n < 815432
  return $ s

-- answer2 = filter foldl (>>=) return (replicate 6 (\x -> [x..9])) $ [2..8]
