module Day1 where

import Data.List
import Data.Maybe
import Control.Monad
import Data.Ord
import Data.Bits
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map as Map

extract = mapMaybe uncons . tails

day1p1 xs = do
    (x, xs1) <- extract xs
    y <- xs1
    guard $ x + y == 2020
    return $ x * y 

day1p2 xs = do
    (x, xs1) <- extract xs
    (y, xs2) <- extract xs1
    z <- xs2
    guard $ x + y + z == 2020
    return $ x * y * z