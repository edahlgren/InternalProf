module Tests where

import Data.Sequence as S
import qualified Data.Foldable as F
import Data.List as L
import Data.ListLike as LL
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
        , ("foldl'", (doList $ foldl'List, doSeq $ foldl'Seq)) 
        , ("concat", (doList $ concatList, doSeq $ concatSeq))
        , ("nubBy", (doList $ nubByList, doSeq $ nubBySeq))
        , ("sortBy", (doList $ sortByList, doSeq $ sortBySeq))
        ]

sortBySeq :: SeqF
sortBySeq xs = LL.sortBy (\x y -> compare x y) xs

sortByList :: ListF
sortByList xs = L.sortBy (\x y -> compare x y) xs

nubBySeq :: SeqF
nubBySeq xs = LL.nubBy (\x y -> x < y) xs

nubByList :: ListF
nubByList xs = L.nubBy (\x y -> x < y) xs

concatSeq :: SeqF
concatSeq xs = xs >< xs

concatList :: ListF
concatList xs = xs ++ xs

partSeq :: SeqF
partSeq xs = fst $ S.partition (\r -> r < 1000) xs

partList :: ListF
partList xs = fst $ L.partition (\r -> r < 1000) xs

foldlSeq :: SeqF
foldlSeq xs = S.empty |> LL.foldl (+) 0 xs
  
foldlList :: ListF
foldlList xs = (L.foldl (+) 0 xs) : []

foldl'Seq :: SeqF
foldl'Seq xs = S.empty |> LL.foldl' (+) 0 xs
  
foldl'List :: ListF
foldl'List xs = (L.foldl' (+) 0 xs) : []

doSeq :: SeqF -> [Int] -> Int -> IO ()
doSeq f steps runBuffer = stepCommand steps runBuffer $ f

doList :: ListF -> [Int] -> Int -> IO ()
doList f steps runBuffer = stepCommand steps runBuffer $ f