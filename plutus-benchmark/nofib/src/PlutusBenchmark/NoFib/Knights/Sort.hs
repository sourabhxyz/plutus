{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module PlutusBenchmark.NoFib.Knights.Sort
        ( insertSort,
          mergeSort,
          quickSort
        ) where

import PlutusTx.Prelude qualified as Tx

insertSort :: (Tx.Ord a) => [a] -> [a]
insertSort xs = foldr insertion [] xs
{-# INLINABLE insertSort #-}

insertion :: (Tx.Ord a) => a -> [a] -> [a]
insertion x [] = [x]
insertion x (y:ys)
        | x Tx.<= y    = x:y:ys
        | otherwise = y:insertion x ys
{-# INLINABLE insertion #-}

mergeSort :: (Tx.Ord a) => [a] -> [a]
mergeSort xs
        = if (n <=1 ) then xs
          else
             (mergeList
                ( mergeSort (take (n `div` 2) xs))
                ( mergeSort (drop (n `div` 2) xs)))
          where
                n = length xs
{-# INLINABLE mergeSort #-}

mergeList :: (Tx.Ord a) => [a] -> [a] -> [a]
mergeList []   ys = ys
mergeList xs   [] = xs
mergeList (x:xs) (y:ys)
        | x Tx.<= y    = x:mergeList xs (y:ys)
        | otherwise = y:mergeList (x:xs) ys
{-# INLINABLE mergeList #-}

quickSort :: (Tx.Ord a) => [a] -> [a]
quickSort []     = []
quickSort (x:xs) = (quickSort [y | y<-xs, y Tx.< x]) ++ [x] ++
                   (quickSort [y | y<-xs, y Tx.>= x])
{-# INLINABLE quickSort #-}

{-% These don't work in Plutus, and aren't used in the original program.
lazySortLe :: (a -> a -> Bool) -> [a] -> [a]
lazySortLe le l = lazyQsort le   l []
{-# INLINABLE lazySortLe #-}

lazySort :: (Tx.Ord a) => [a] -> [a]
lazySort      l = lazyQsort (<=) l []
{-# INLINABLE lazySort #-}

-- lazyQsort is stable and does not concatenate.
lazyQsort :: (a -> a -> Bool) -> [a] -> [a] -> [a]
lazyQsort le []     r = r
lazyQsort le [x]    r = x:r
lazyQsort le (x:xs) r = qpart le x xs [] [] r
{-# INLINABLE lazyQsort #-}

-- rlazyQsort is as lazyQsort but anti-stable,
-- i.e. reverses equal elements.
rlazyQsort :: (a -> a -> Bool) -> [a] -> [a] -> [a]
rlazyQsort  le []     r = r
rlazyQsort le [x]    r  = x:r
rlazyQsort  le (x:xs) r = rqpart le x xs [] [] r
{-# INLINABLE rlazyQsort #-}

-- qpart partitions and sorts the sublists
-- rlt and rge are in reverse order and must be sorted with an
-- anti-stable sorting
qpart :: (a -> a -> Bool) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
qpart le x [] rlt rge r =
    rlazyQsort le rlt (x:rlazyQsort le rge r)
qpart le x (y:ys) rlt rge r =
    if le x y then
        qpart le x ys rlt (y:rge) r
    else
        qpart le x ys (y:rlt) rge r
{-# INLINABLE qpart #-}

rqpart :: (a -> a -> Bool) -> a -> [a] -> [a] -> [a] -> [a] -> [a]
rqpart le x [] rle rgt r =
    lazyQsort le rle (x:lazyQsort le rgt r)
rqpart le x (y:ys) rle rgt r =
    if le y x then
        rqpart le x ys (y:rle) rgt r
    else
        rqpart le x ys rle (y:rgt) r
{-# INLINABLE rqpart #-}
%-}

randomIntegers :: Integer -> Integer -> [Integer]
randomIntegers s1 s2 =
    if 1 <= s1 && s1 <= 2147483562 then
        if 1 <= s2 && s2 <= 2147483398 then
            rands s1 s2
        else
            error "randomIntegers: Bad second seed."
    else
        error "randomIntegers: Bad first seed."
{-# INLINABLE randomIntegers #-}

rands :: Integer -> Integer -> [Integer]
rands s1 s2
   = if z < 1 then z + 2147483562 : rands s1'' s2''
     else
         z : rands s1'' s2''
     where
        k    = s1 `div` 53668
        s1'  = 40014 * (s1 - k * 53668) - k * 12211
        s1'' = if s1' < 0 then s1' + 2147483563 else s1'

        k'   = s2 `div` 52774
        s2'  = 40692 * (s2 - k' * 52774) - k' * 3791
        s2'' = if s2' < 0 then s2' + 2147483399 else s2'

        z    = s1'' - s2''
{-# INLINABLE rands #-}

-- % These are from the original program.  That's literate Haskell, and it
-- % contains the results as latex.

test1,test2,test3,test4,test5,test6,test7::[Integer]

test1 = [1..10]
test2 = [10,9..1]
test3 = [1..500]
test4 = [500,499..1]

test5 = take 10   (randomIntegers 123213 342234)
test6 = take 100  (randomIntegers 123213 342234)
test7 = take 1000 (randomIntegers 123213 342234)

