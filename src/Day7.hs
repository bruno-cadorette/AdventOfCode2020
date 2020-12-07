{-# LANGUAGE TypeFamilies, TupleSections #-}
module Day7 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Void
import Data.Maybe
import Data.Fix
import Data.Tree
import Data.List
import Data.Coerce

newtype BagColor = BagColor String deriving (Show, Eq, Ord)

type Parser = Parsec Void String

color :: Parser BagColor
color = do
  w1 <- many letterChar
  char ' '
  w2 <- many letterChar
  char ' '
  (string "bags" <|> string "bag")
  return $ BagColor (w1 ++ " " ++ w2)

additionalColor = do
  n <- decimal
  char ' '
  c <- color
  return (n, c)

noBags = string "no other bags" >> return []

parseLine :: Parser (BagColor, [(Int, BagColor)])
parseLine = do
  bagColor <- color
  string " contain "
  xs <- noBags <|> additionalColor `sepBy` string ", "
  char '.'
  return (bagColor, xs)

root = BagColor "shiny gold"

createMapP1 :: [(BagColor, [(Int, BagColor)])] -> Map.Map BagColor [BagColor]
createMapP1 = 
  Map.fromListWith (++) . 
  concatMap (\(bag, corr) -> fmap ((,[bag]). snd) corr)

createMapP2 :: [(BagColor, [(Int, BagColor)])] -> Map.Map BagColor [(Int, BagColor)]
createMapP2 =
  Map.fromListWith (++)
  

createTreeP1 :: Ord a => a -> Map.Map a [a] -> Tree a
createTreeP1 root m = 
  case Map.lookup root m of
    Just xs -> Node root (fmap (\x -> createTreeP1 x m) xs)
    Nothing -> Node root [] 

createTreeP2 :: Ord a => (Int, a) -> Map.Map a [(Int, a)] -> Tree (Int, a)
createTreeP2 root m = 
  case Map.lookup (snd root) m of
    Just xs -> Node root (fmap (\x -> createTreeP2 x m) xs)
    Nothing -> Node root [] 

solveP2 :: Int -> Tree Int -> Int
solveP2 mult (Node n xs) = n' + sum (fmap (solveP2 n') xs)
  where n' = n * mult
    
parseFile = fromJust . parseMaybe (parseLine `sepEndBy` newline)


day7p1 = pred . length . nub . flatten . createTreeP1 root . createMapP1 . parseFile
day7p2 = pred . solveP2 1 . fmap fst . createTreeP2 (1, root) . createMapP2 . parseFile
--testt = parseTest (string "bags" <|> string "bag") "bag"