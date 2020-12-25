module Day1.B where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.List (List, sort, reverse, (:))
import Data.Tuple (Tuple(..))
import Day1.Numbers (numbers)

main :: Effect Unit
main = do
  log "The correct number is:"
  let Tuple a b = solve sortedNumbers sortedLargeNumbers
  log $ show (a * b)

solve :: List Int -> List Int -> Tuple Int Int
solve (x:xs) (y:ys) = case compare (x + y) 2020 of
    GT -> solve (x:xs) ys
    EQ -> Tuple x y
    LT -> solve xs (y:ys)
solve _ _ = Tuple 0 0

sortedNumbers :: List Int
sortedNumbers = sort numbers

sortedLargeNumbers :: List Int
sortedLargeNumbers = reverse sortedNumbers