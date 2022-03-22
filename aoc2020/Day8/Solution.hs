import Text.ParserCombinators.ReadP (readP_to_S, char, string, many1, choice, eof)
import Control.Applicative ((<|>))
import Control.Monad.State (get, put, evalState)
import Data.List (find)

main = do
  i <- readFile "input.txt"
  let instructions = zip [0..] . fmap parse . lines $ i
--  print $ evalState run (0, 0, [], instructions)
  let vars = variations instructions
  let haltIns = length instructions
  print $ find g $ map (\v -> makeStates v haltIns) vars

g (Just _) = True
g _ = False

makeStates v haltIns = evalState run (0, 0, [], v, haltIns)

variations i = evalState (mapM f (repeat i)) 0
  where f i = do
          n <- get
          let (newN, newI) = findAR n i
          put newN
          pure newI

findAR n [] = error "no more instructions"
findAR n ((m, ("nop", f)):xs) | m >= n = (m + 1, (m, ("jmp", f)):xs)
findAR n ((m, ("jmp", f)):xs) | m >= n = (m + 1, (m, ("nop", f)):xs)
findAR n (x:xs) = let (newN, rest) = findAR n xs in (newN, x:rest)

run = do
  (i, acc, prev, l, h) <- get
  case lookup i l of
    Just ("nop", f) -> let newI = i + 1 in if newI `elem` prev
                                           then pure Nothing
                                           else if newI == h
                                                then pure $ Just acc
                                                else put (newI, acc, newI:prev, l, h) >> run
    Just ("acc", f) -> let newI = i + 1 in if newI `elem` prev
                                           then pure Nothing
                                           else if newI == h
                                                then pure $ Just acc
                                                else put (newI, f acc, newI:prev, l, h) >> run
    Just ("jmp", f) -> let newI = f i in if newI `elem` prev
                                         then pure Nothing
                                         else if newI == h
                                              then pure $ Just acc
                                              else put (newI, acc, newI:prev, l, h) >> run
    Nothing -> error "No instruction in the list"

  {-
run = do
  (i, acc, prev, l) <- get
  case lookup i l of
    Just ("nop", f) -> let newI = i + 1 in if newI `elem` prev then pure acc else put (newI, acc, newI:prev, l) >> run 
    Just ("acc", f) -> let newI = i + 1 in if newI `elem` prev then pure acc else put (newI, f acc, newI:prev, l) >> run
    Just ("jmp", f) -> let newI = f i in if newI `elem` prev then pure acc else put (newI, acc, newI:prev, l) >> run
    Nothing -> error "No instruction in the list"
-}
parse :: String -> (String, Int -> Int)
parse = fst . head . readP_to_S line
  where line = do
          ins <- instruction
          _ <- char ' '
          arg <- argument
          _ <- eof
          pure (ins, arg)
        instruction = string "nop" <|> string "acc" <|> string "jmp"
        argument = pos <|> neg
        pos = char '+' >> number >>= \n -> pure (\x -> x + read n)
        neg = char '-' >> number >>= \n -> pure (\x -> x - read n)
        
letter = choice . map char $ ['a'..'z']
digit = choice . map char $ ['0'..'9']
number = many1 digit
