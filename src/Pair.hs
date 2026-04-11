module Pair
  ( Pair (..),
    pair,
    pairContains,
    pairFromList,
    getAdjacentTuples,
    getAdjacentPairs,
  )
where

data Pair a = Pair a a
  deriving (Eq, Ord, Show)

pair :: (Ord a) => a -> a -> Pair a
pair x y = if x > y then Pair y x else Pair x y

pairContains :: (Eq a) => a -> Pair a -> Bool
pairContains x (Pair p1 p2) = x == p1 || x == p2

pairFromList :: (Ord a) => [a] -> Pair a
pairFromList [x, y] = pair x y
pairFromList _ = undefined

getAdjacentTuples :: [a] -> [(a, a)]
getAdjacentTuples xs = getAdjacentTuples' xs
  where
    firstElem = head xs
    getAdjacentTuples' (nextElem : nextNextElem : restElems) =
      (nextElem, nextNextElem) : getAdjacentTuples' (nextNextElem : restElems)
    getAdjacentTuples' [lastElem] = [(lastElem, firstElem)]
    getAdjacentTuples' [] = undefined

getAdjacentPairs :: (Ord a) => [a] -> [Pair a]
getAdjacentPairs xs = [pair x1 x2 | (x1, x2) <- getAdjacentTuples xs]