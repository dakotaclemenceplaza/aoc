import Control.Monad.State (get, put, evalState, join, State)
import Data.List (find)
import Data.Maybe (isJust)

main = do
  i <- readFile "input.txt"
  let input = fmap read . lines $ i
  case findAnswer input of
    Just num -> do
      print num
      print $ findAnswer2 num (take 2 input) (sum $ take 2 input) (drop 2 input)
    Nothing -> print "No answer to fisrt part"
    
findAnswer i = join $ find isJust $ evalState (mapM go . drop 25 $ i) (take 25 i)
  where allSums [] = []
        allSums (x:xs) = map (+x) (filter (/=x) xs) <> allSums xs
        go :: Int -> State [Int] (Maybe Int)
        go n = do
          s <- get
          if n `elem` allSums s
            then put (tail s ++ [n]) >> pure Nothing
            else pure $ Just n

findAnswer2 :: Int -> [Int] -> Int -> [Int] -> Int
findAnswer2 n ms s xs | s > n = findAnswer2 n (tail ms) (s - head ms) xs
findAnswer2 n ms s xs = case go ms s xs of
                            (False, a, b, s) -> findAnswer2 n (tail a) (s - head ms) b
                            (True, a, _, _) -> maximum a + minimum a
  where go ms s xs | s == n = (True, ms, xs, s)
        go ms s (x:xs) | s + x > n = (False, ms, (x:xs), s)
        go ms s (x:xs) = go (ms <> [x]) (s + x) xs
