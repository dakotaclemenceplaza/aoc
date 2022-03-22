module Day3.Solution where

import Text.ParserCombinators.ReadP
import Control.Applicative
import qualified Control.Monad.State as S
import Data.List (find)

main :: IO ()
main = readFile "input.txt" >>= doIt . prepare

-- for debug
printR s = print s >> pure s

prepare = fmap (reverse . makePath . parse) . lines . init

doIt s = do
  let points = findPoints s
  let p1p2 = fmap makeWholePath s
  let [p1, p2] = p1p2
  print $ findSteps points p1 p2

type Segment = ((Int, Int), (Int, Int))
  
makePath :: [PathLine] -> [Segment]
makePath l = S.execState w [((0, 0), (0, 0))]
  where w = mapM_ f l
        f :: PathLine -> S.State [Segment] ()
        f pathLine = do
          s <- S.get
          let a@((_, current):_) = s
          S.put $ go pathLine current : a
        go :: PathLine -> (Int, Int) -> Segment
        go (U n) (x, y) = ((x, y), (x, y + n))
        go (R n) (x, y) = ((x, y), (x + n, y))
        go (D n) (x, y) = ((x, y), (x, y - n))
        go (L n) (x, y) = ((x, y), (x - n, y))


makeWholePath :: [Segment] -> [(Int, (Int, Int))]
makeWholePath l = (0, (0, 0)) : (zip [1..] $ concatMap f l)
  where f ((a, b), (c, d)) | a == c && b < d = zip (repeat a) [(b+1)..d]
        f ((a, b), (c, d)) | a == c = zip (repeat a) (reverse [d..(b-1)])
        f ((a, b), (c, d)) | a < c = zip [(a+1)..c] (repeat b)
        f ((a, b), (c, d)) = zip (reverse [c..(a-1)]) (repeat b)
        
findSteps :: [(Int, Int)] -> [(Int, (Int, Int))] -> [(Int, (Int, Int))] -> Int
findSteps points path1 path2  = minimum $ foldl g [] filtered1
  where filtered1 = filter f path1
        filtered2 = filter f path2
        f (c, p) = p `elem` points
        g acc v = case find (\(_, p) -> p == snd v) filtered2 of
                    Just (n, _) -> (n + fst v) : acc
                    Nothing -> acc

findPoints :: [[Segment]] -> [(Int, Int)]
findPoints [l1, l2] = foldl f [] l1
  where f acc v = case filter (fuckingPredicateLoL v) l2 of
          [] -> acc
          l -> acc ++ map g l
            where g ((a, b), (c, d)) | a == c = (a, snd $ fst v)
                  g ((a, b), (c, d)) | b == d = (fst $ snd v, b)

nearest l = minimum $ map f l
  where f (x, y) = abs x + abs y
          
fuckingPredicateLoL ((a, b), (c, d)) ((n, m), (o, p)) = (((n > a && n < c || n < a && n > c) || (o > a && o < c || o < a && o > c)) && (m >= b && p <= b || m <= b && p >= b))
                                                        ||
                                                        (((m > b && m < d || m < b && m > d) || (p > b && p < d || p < b && p > d)) && (n >= a && o <= a || n <= a && o >= a))

countSteps (segs1, segs2) points = undefined

data PathLine = U Int | R Int | D Int | L Int deriving (Show, Eq)

parse = fst . head . readP_to_S finalParser
  where finalParser = manyTill (item <|> lastItem) eof
        item = do
          d <- direction
          n <- number
          _ <- char ','
          return $ chooseDirection d $ read n
        lastItem = do
          d <- direction
          n <- number
          _ <- eof
          return $ chooseDirection d $ read n
        direction = char 'U' <|> char 'R' <|> char 'D' <|> char 'L'
        number = many1 . choice $ map char "0123456789"
        chooseDirection 'U' = U
        chooseDirection 'R' = R
        chooseDirection 'D' = D
        chooseDirection 'L' = L
        chooseDirection _ = error "impossible direction"
