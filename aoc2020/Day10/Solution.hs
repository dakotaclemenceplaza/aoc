import Data.List (sort)
import Control.Monad.State (get, put, execState, foldM, State)
import Data.Functor.Foldable
import Control.Comonad.Cofree (Cofree(..))

main = do
  i <- readFile "input.txt"
  let adapters :: [Int]
      adapters = sort . fmap read . lines $ i
      diffs = 3 : execState (foldM f 0 adapters) [] 
  print $ length (filter (==1) diffs) * length (filter (==3) diffs)
  print $ histo g (0:adapters)
  
f :: Int -> Int -> State [Int] Int
f acc x = do
  s <- get
  put $ x - acc : s
  pure x

g Nil = 1
g (Cons v (xacc :< (Cons _ (yacc :< (Cons y (zacc :< (Cons z _))))))) | y - v <= 3 && z - v <= 3 = xacc + yacc + zacc
g (Cons v (xacc :< (Cons _ (yacc :< (Cons y _))))) | y - v <= 3 = xacc + yacc
g (Cons _ (xacc :< _)) = xacc
