module Day8 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Void
import Data.Maybe
import Data.Tree
import Data.List
import Data.List.Split
import Data.Coerce
import Data.Array

type Parser = Parsec Void String

data Instruction = Acc Int | Jmp Int | Nop Int deriving (Show)
data Exit = InfiniteLoop Int | OutOfBound | Success Int deriving (Show)
isSuccess (Success _) = True
isSuccess _ = False

data Program = Program {
  instructions :: (Array Int Instruction),
  programPointer :: Int,
  accumulator :: Int
  } deriving (Show)
jump n p = p {programPointer = programPointer p + n}
accumulate n p = p {accumulator = accumulator p + n}
currentInstruction p = instructions p ! programPointer p 

instruction = 
  Nop <$ string "nop" <|>
  Acc <$ string "acc" <|>
  Jmp <$ string "jmp"

argument = do
  signFunction <- id <$ char '+' <|> negate <$ char '-'
  n <- decimal
  return $ signFunction n

line :: Parser Instruction
line = do
  ins <- instruction
  space
  arg <- argument
  return $ ins arg

parseProgram file = Program (listArray (0, length xs) xs) 0 0
  where xs = fromJust $ parseMaybe (line `sepEndBy` newline) file

interpreter (Acc n) = jump 1 . accumulate n
interpreter (Jmp n) = jump n
interpreter (Nop _) = jump 1

lastProgramPointer = snd . bounds . instructions

runProgram :: Set.Set Int -> Program -> Exit
runProgram cache p 
  | Set.member (programPointer p) cache = InfiniteLoop (accumulator p)
  | lastProgramPointer p == programPointer p = Success (accumulator p)
  | lastProgramPointer p < programPointer p = OutOfBound
  | otherwise =
      runProgram (Set.insert (programPointer p) cache) (interpreter (currentInstruction p) p) 

possibleStateChange (Jmp n) = Just $ Nop n
possibleStateChange (Nop n)
  | n /= 0 = Just $ Jmp n
possibleStateChange _ = Nothing

allPossiblePrograms p = fmap (\ins -> Program ins 0 0) $ mapMaybe (\i -> fmap (\x -> arr//[(i, x)]) $ possibleStateChange(arr!i)) [a..b]
  where 
    (a, b) = bounds arr
    arr = instructions p 
      

day8p1 = runProgram Set.empty . parseProgram
day8p2 = find isSuccess . fmap (runProgram Set.empty) . allPossiblePrograms . parseProgram