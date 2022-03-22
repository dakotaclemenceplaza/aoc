module Day7.Solution where

import Day5.Solution (run)
import Util.Util (parse)
import Control.Monad.Trans.State
import Data.List (permutations, nub)

main :: IO ()
main = readFile "Day7/input.txt" >>= print . solve . parse

runAmpLine :: [Int] -> [Int] -> Int
runAmpLine prog phases = evalState ampLine phases
  where ampLine = foldl (>>=) (return 0) (map makeAmp [0..4])
        makeAmp n i = do
          phs <- get
          return $ head $ run prog [phs !! n, i]

solve prog = maximum
             . map (runAmpLine prog)
             . filter (\l -> nub l == l)
             . permutations $ [0..4]
