module Day5.Solution where

import Text.ParserCombinators.ReadP (readP_to_S, char,many1, choice, eof, optional)
import Control.Applicative ((<|>))
import Control.Monad.Trans.State
import Util.Util (parse, replace)

main :: IO ()
main = readFile "Day5/input.txt" >>= print . flip run [5] . parse

type Program = [Int]
type Input = [Int]
type Output = [Int]

data TheState = TheState
  { prog :: Program
  , input :: Input
  }

run :: Program -> Input -> Output
run p i = evalState (run' 0) (TheState p i)
  where run' :: Int -> State TheState Output
        run' ip = do
          s <- get
          let program = prog s
              opCode = program !! ip
          case parseOp opCode of
            IoOp Input -> do
              let address = program !! (ip + 1)
              put $ s { input = tail $ input s }
              store (head $ input s) address
              run' (ip + 2)
            IoOp (Output mode) -> do
              value <- getModeFunc mode (ip + 1)
              nextOutput <- run' (ip + 2)
              return $ value : nextOutput
            MathOp mode1 mode2 Add -> arithmetic (+) mode1 mode2 ip
            MathOp mode1 mode2 Mult -> arithmetic (*) mode1 mode2 ip
            JOp mode1 mode2 JifT -> jump (/=) mode1 mode2 ip
            JOp mode1 mode2 JifF -> jump (==) mode1 mode2 ip
            Cond mode1 mode2 LessThen -> conditional (<) mode1 mode2 ip
            Cond mode1 mode2 Equal -> conditional (==) mode1 mode2 ip
            Halt -> return []
        arithmetic operation mode1 mode2 ip = do
          x <- getModeFunc mode1 (ip + 1)
          y <- getModeFunc mode2 (ip + 2)
          s <- get
          let address = prog s !! (ip + 3)
          store (x `operation` y) address
          run' (ip + 4)
        jump operation mode1 mode2 ip = do
          value <- getModeFunc mode1 (ip + 1)
          if value `operation` 0
            then getModeFunc mode2 (ip + 2) >>= run'
            else run' (ip + 3)
        conditional operation mode1 mode2 ip = do
          x <- getModeFunc mode1 (ip + 1)
          y <- getModeFunc mode2 (ip + 2)
          s <- get
          let address = prog s !! (ip + 3)
          if x `operation` y
            then store 1 address >> run' (ip + 4)
            else store 0 address >> run' (ip + 4)
        getModeFunc Position = \ip -> do
          s <- get
          return $ prog s !! (prog s !! ip)
        getModeFunc Immediate = \ip -> do
          s <- get
          return $ prog s !! ip
        store value address = do
          s <- get
          put $ s { prog = replace value address (prog s) }
        
data InOut = Input | Output Mode deriving (Show)
data Math = Add | Mult deriving (Show)
data Mode = Position | Immediate deriving (Show)
data Jump = JifT | JifF deriving (Show)
data Conditional = LessThen | Equal deriving (Show)
data Op = IoOp InOut
        | MathOp Mode Mode Math
        | JOp Mode Mode Jump
        | Cond Mode Mode Conditional
        | Halt
  deriving (Show)

parseOp :: Int -> Op
parseOp n = fst . head . readP_to_S finalParser $ show n
  where finalParser = choice [a1, a2, a3,
                              b1, b2, b3,
                              c1, c2, c3,
                              op4Mode, op4NoMode, input, halt]
        a1 = do
          _ <- optional digits
          mode2 <- pos <|> imm
          mode1 <- pos <|> imm
          _ <- digit
          op <- op1 <|> op2
          return $ MathOp mode1 mode2 op
        a2 = do
          _ <- optional digits
          mode1 <- pos <|> imm
          _ <- digit
          op <- op1 <|> op2
          return $ MathOp mode1 Position op
        a3 = do
          _ <- optional digits
          op <- op1 <|> op2
          return $ MathOp Position Position op
        b1 = do
          _ <- optional digits
          mode2 <- pos <|> imm
          mode1 <- pos <|> imm
          _ <- digit
          op <- op5 <|> op6
          return $ JOp mode1 mode2 op
        b2 = do
          _ <- optional digits
          mode1 <- pos <|> imm
          _ <- digit
          op <- op5 <|> op6
          return $ JOp mode1 Position op
        b3 = do
          _ <- optional digits
          op <- op5 <|> op6
          return $ JOp Position Position op
        c1 = do
          _ <- optional digits
          mode2 <- pos <|> imm
          mode1 <- pos <|> imm
          _ <- digit
          op <- op7 <|> op8
          return $ Cond mode1 mode2 op
        c2 = do
          _ <- optional digits
          mode1 <- pos <|> imm
          _ <- digit
          op <- op7 <|> op8
          return $ Cond mode1 Position op
        c3 = do
          _ <- optional digits
          op <- op7 <|> op8
          return $ Cond Position Position op
        op4Mode = do
          _ <- optional digits
          mode <- pos <|> imm
          _ <- digit
          _ <- op4
          return (IoOp $ Output mode)
        op4NoMode = do
          _ <- optional digits >> op4
          return (IoOp $ Output Position)
        input = optional digits >> op3
        pos = char '0' >> return Position
        imm = char '1' >> return Immediate
        op1 = char '1' >> eof >> return Add
        op2 = char '2' >> eof >> return Mult
        op3 = optional digits >> char '3' >> eof >> return (IoOp Input)
        op4 = char '4' >> eof >> return Output
        op5 = char '5' >> eof >> return JifT
        op6 = char '6' >> eof >> return JifF
        op7 = char '7' >> eof >> return LessThen
        op8 = char '8' >> eof >> return Equal
        halt = optional digits >> char '9' >> char '9' >> eof >> return Halt
        digit = choice $ fmap char "0123456789"
        digits = many1 digit
