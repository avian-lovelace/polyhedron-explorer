module Pair
  ( Pair (..),
    pair,
    pairFromList,
    getAdjacentPairs,
  )
where

data Pair a = Pair a a
  deriving (Eq, Ord, Show)

pair :: (Ord a) => a -> a -> Pair a
pair x y = if x > y then Pair y x else Pair x y

pairFromList :: (Ord a) => [a] -> Pair a
pairFromList [x, y] = pair x y
pairFromList _ = undefined

getAdjacentPairs :: (Ord a) => [a] -> [Pair a]
getAdjacentPairs xs = getAdjacentPairs' [] xs
  where
    firstElem = head xs
    getAdjacentPairs' currentPairs (nextElem : nextNextElem : restElems) =
      getAdjacentPairs' (pair nextElem nextNextElem : currentPairs) (nextNextElem : restElems)
    getAdjacentPairs' currentPairs [lastElem] = pair lastElem firstElem : currentPairs
    getAdjacentPairs' _currentPairs [] = undefined