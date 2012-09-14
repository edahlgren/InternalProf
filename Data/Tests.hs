module Tests where

import Data.Sequence as S
import qualified Data.Foldable as F
import qualified  Data.List as L
import qualified Data.ListLike as LL
import System.IO

import Sequence

type DoTest = [Int] -> Int -> IO ()

type SeqF = Seq Int -> Seq Int
type ListF = [Int] -> [Int]

instance Test [Int] where
    builder n = F.toList $ iterSeq n S.empty

instance Test (Seq Int) where
    builder n = iterSeq n S.empty

iterSeq 0 sequence = sequence
iterSeq n sequence = iterSeq (n - 1)(sequence |> n)

tests :: [ (String, (DoTest, DoTest)) ]
tests = [ ("partition", (doList $ partList, doSeq $ partSeq))
        , ("foldl", (doList $ foldlList, doSeq $ foldlSeq)) 
        , ("concat", (doList $ concatList, doSeq $ concatSeq))
        , ("nubBy", (doList $ nubByList, doSeq $ nubBySeq))
        , ("sortBy", (doList $ sortByList, doSeq $ sortBySeq))
        ]

{- Data.ListLike
sortBy :: (item -> item -> Ordering) -> full -> full 
sortBy cmp = foldr (insertBy cmp) empty

insertBy :: (item -> item -> Ordering) -> item -> full -> full 
insertBy cmp x ys
    | null ys = singleton x
    | otherwise = case cmp x (head ys) of
                        GT -> cons (head ys) (insertBy cmp x (tail ys))
                        _ ->  cons x ys
-}

sortBySeq :: SeqF
sortBySeq xs = LL.sortBy (\x y -> compare x y) xs

{- Data.List
sortBy cmp = foldr (insertBy cmp) []

insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys
-}

sortByList :: ListF
sortByList xs = L.sortBy (\x y -> compare x y) xs

{- Data.ListLike
nubBy f l = nubBy' l (empty :: full)
     where
      nubBy' ys xs
        | null ys              = empty
        | any (f (head ys)) xs = nubBy' (tail ys) xs
        | otherwise            = let y = head ys
                                 in  cons y (nubBy' (tail ys) (cons y xs))
-}

nubBySeq :: SeqF
nubBySeq xs = LL.nubBy (\x y -> x < y) xs

{- Data.List
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
nubBy eq l              = nubBy' l []
  where
    nubBy' [] _         = []
    nubBy' (y:ys) xs
       | elem_by eq y xs = nubBy' ys xs
       | otherwise       = y : nubBy' ys (y:xs)
-}

nubByList :: ListF
nubByList xs = L.nubBy (\x y -> x < y) xs

{- Data.Sequence
Seq xs >< Seq ys = Seq (appendTree0 xs ys)
-- see below
-}

concatSeq :: SeqF
concatSeq xs = xs >< xs

{- GHC.Base
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
-}

concatList :: ListF
concatList xs = xs ++ xs

{- Data.ListLike
partition :: (item -> Bool) -> full -> (full, full)
partition p xs = (filter p xs, filter (not . p) xs)
-}

partSeq :: SeqF
partSeq xs = fst $ LL.partition (\r -> r < 1000) xs

{- Data.List
partition               :: (a -> Bool) -> [a] -> ([a],[a])
{-# INLINE partition #-}
partition p xs = foldr (select p) ([],[]) xs

select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)
-}

partList :: ListF
partList xs = fst $ L.partition (\r -> r < 1000) xs

{- Data.Sequence -> Foldable
foldl f z (Seq xs) = foldl (foldl f) z xs
-}

foldlSeq :: SeqF
foldlSeq xs = S.empty |> F.foldl (+) 0 xs

{- GHC.List
foldl        :: (a -> b -> a) -> a -> [b] -> a
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs
-}
              
foldlList :: ListF
foldlList xs = (L.foldl (+) 0 xs) : []

doSeq :: SeqF -> [Int] -> Int -> IO ()
doSeq f steps runBuffer = stepCommand steps runBuffer $ f

doList :: ListF -> [Int] -> Int -> IO ()
doList f steps runBuffer = stepCommand steps runBuffer $ f