module Day2.Solution where

main :: IO ()
main = readFile "input.txt" >>= print . find . fmap read . parse . init

parse "" = []
parse (',':xs) = ",":parse xs
parse (x:xs) = case parse xs of
                 [] -> [[x]]
                 (",":ys) -> [x]:ys
                 (y:ys) -> (x:y):ys

run :: [Int] -> Int
run li = run' 0 li 
  where run' p l = let op = l !! p
                       x = l !! (p + 1)
                       y = l !! (p + 2)
                       z = l !! (p + 3)
                   in case op of
                        1 -> run' (p + 4) (replace l z (l !! x + l !! y))
                        2 -> run' (p + 4) (replace l z (l !! x * l !! y))
                        99 -> head l
                        _ -> error "something went wrong"

replace l i v = let (a, b) = splitAt i l
                 in a ++ [v] ++ tail b

inputs = [(x, y) | x <- [0..99], y <- [0..99]]

find l = 100 * noun + verb
  where f (x, y) = ((x, y), run (replace (replace l 1 x) 2 y))
        (noun, verb) = fst . head $ filter (\(a, b) -> b == 19690720) $ map f inputs

-- 3224742
-- 7960

