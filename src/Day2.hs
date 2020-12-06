module Day2 where

import Data.List
import Data.Maybe
import Control.Monad
import Data.Ord
import Data.Bits
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map as Map


policyPart1 minimumLenght maximumLenght character password = minimumLenght <= policy && policy <= maximumLenght
    where
        policy = length $ filter (== character) password
        
policyPart2 pos1 pos2 character password = 
    (password !! (pos1 - 1) == character) `xor` 
    (password !! (pos2 - 1) == character)
parseInput = fmap (words . fmap (\x -> if x == '-' || x == ':' then ' ' else x)) . lines
solveDay2 respectPolicy = length . filter (\[a,b,c,d] -> respectPolicy (read a) (read b) (head c) d) . parseInput

day2p1 = solveDay2 policyPart1
day2p2 = solveDay2 policyPart2