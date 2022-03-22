module Day6.Solution where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.List (find)

main :: IO ()
main = readFile "input.txt" >>= print . solve2 . parse

solve parsed = foldl (\acc (_, n) -> acc + n) 0 planets
  where planets = f ("COM", 0)
        f (p, n) = case filter (\(a, _) -> a == p) parsed of
          [] -> []
          items -> let itemss = map (\(_, b) -> (b, n + 1)) items
                   in itemss ++ concatMap f itemss

solve2 :: [(String, String)] -> Maybe Int
solve2 parsed = go "YOU" 0
  where go :: String -> Int -> Maybe Int
        go orbits n = let orbited = find (equals orbits . snd) parsed
                 in case orbited of
                      Nothing -> Nothing
                      Just ("SAN", _) -> Just n
                      Just (orbited, orbits) -> case filter (equals orbited . fst) parsed of
                        [_] -> go orbited (n + 1)
                        l -> let blah = filter (notEquals orbits . snd) l
                                 blah2 = map (searchBranch parsed) blah
                             in if all (==Nothing) blah2
                                then go orbited (n + 1)
                                else fmap minimum . sequence $ map (fmap (+n)) $ filter fuckingHelper blah2

fuckingHelper Nothing = False
fuckingHelper _ = True

equals v = (==v)
notEquals v = (/=v)

searchBranch :: [(String, String)] -> (String, String) -> Maybe Int
searchBranch l p = go p 0
  where go (a, "SAN") n = Just n
        go (a, b) n = case filter (equals b . fst) l of
          [] -> Nothing
          bs -> let aaa = map (\bb -> go bb (n + 1)) bs
                in if all (==Nothing) aaa
                   then Nothing
                   else fmap minimum . sequence $ filter fuckingHelper aaa
                
parse = fst . head . readP_to_S finalParser
  where finalParser = sepBy item (char '\n') >>= \items -> char '\n' >> eof >> return items
        item = do
          p1 <- planet
          _ <- orbits
          p2 <- planet
          return (p1, p2)
        orbits = char ')'        
        planet = count 3 (digit <|> letter)
        digit = choice $ fmap char ['0'..'9']
        letter = choice $ fmap char ['A'..'Z']
