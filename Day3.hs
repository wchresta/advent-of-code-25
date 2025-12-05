{-# OPTIONS_GHC -fno-warn-x-partial #-}
module Main where

import AOC
import Data.List (sort, tails)
import Data.Char (digitToInt)
import Debug.Trace

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=3
  , solvers=
    [ WithTests
        (AOC.LineSolver (process 2) collect)
        [("test1", "357")]
    , WithTests
        (AOC.LineSolver (process 12) collect)
        [("test1", "3121910778619")]
    ]
  })

process :: Int -> String -> Int
process n = findMaxBat n . parse

parse :: String -> [Int]
parse = map digitToInt

posVal :: [Int] -> Int
posVal = posVal' . reverse
  where
    posVal' [] = 0
    posVal' [x] = x
    posVal' (x:xs) = x + 10 * posVal' xs

findMaxBat :: Int -> [Int] -> Int
findMaxBat n = posVal . findMaxSeq n

findMaxSeq :: Int -> [Int] -> [Int]
findMaxSeq n as | length as < n = []
findMaxSeq 1 as = [maximum as]
findMaxSeq n as = let
  tailsnum = length as - n + 1
  maxa = maximum . take tailsnum $ as
  subs = filter ((maxa==) . head) . take tailsnum . tails $ as
  maxSeqs = map ((maxa:) . findMaxSeq (n-1) . tail) subs
  -- Best is the longest, or the highest
  best = maxBy (\as -> (length as, posVal as)) maxSeqs
 in
  best


maxBy :: (Ord a, Ord b) => (a -> b) -> [a] -> a
maxBy f as = snd . maximum . map (\x -> (f x, x)) $ as

collect :: [Int] -> String
collect = show . sum


