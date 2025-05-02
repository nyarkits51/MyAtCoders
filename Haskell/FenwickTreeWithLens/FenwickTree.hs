{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Fenwick Tree (Binary Indexed Tree) implementation using lens for functional updates.
module Main
  ( Fenwick,
    fromListToFW,
    updateFW,
    prefixSum,
    rangeSum,
    main,
  )
where

import Control.Lens
import Data.Bits ((.&.))
import Data.List qualified as L
import Data.Sequence qualified as Seq

-- | Fenwick tree storing cumulative sums in a 1-based vector.
data Fenwick a = Fenwick {_tree :: Seq.Seq a}
  deriving (Show, Eq, Ord)

makeLenses ''Fenwick

-- | Build a Fenwick tree from an initial list of values (1-based indexing).
fromListToFW :: (Num a) => [a] -> Fenwick a
fromListToFW xs =
  -- start with zero tree of length n+1, then add each value
  L.foldl'
    (\fw (i, v) -> updateFW i v fw)
    (Fenwick $ Seq.replicate (L.length xs + 1) 0)
    (L.zip [1 ..] xs)

-- | Increase the element at index i by v (1-based).
updateFW :: (Num a) => Int -> a -> Fenwick a -> Fenwick a
updateFW i v fw = over tree (go i) fw
  where
    n = views tree Seq.length fw - 1
    go j vec
      | j <= n =
          let vec' = vec & ix j %~ (+ v)
           in go (j + (j .&. (-j))) vec'
      | otherwise = vec

-- | Compute the prefix sum from 1 to i (inclusive).
{-# INLINE prefixSum #-}
prefixSum :: (Num a) => Int -> Fenwick a -> a
prefixSum i fw = go i 0
  where
    vec = view tree fw
    go 0 acc = acc
    go j acc = go (j - (j .&. (-j))) (acc + vec `Seq.index` j)

-- | Compute the sum of the range [l, r].
rangeSum :: (Num a) => Int -> Int -> Fenwick a -> a
rangeSum l r fw = prefixSum r fw - prefixSum (l - 1) fw

main :: IO ()
main = do
  let ft = fromListToFW [1, 2, 3, 4, 5] :: Fenwick Int
  print $ prefixSum 3 ft
  let ft' = updateFW 2 5 ft
  print $ prefixSum 3 ft'
  print $ rangeSum 2 5 ft'
