module Main where

import AOC
import Data.List (group, sort)
import Data.List.Split (splitOn)
import Debug.Trace

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=2
  , solvers=
    [ WithTests
        (AOC.BlockSolver (solve isInvalid1)) [("test1", "1227775554")]
    , WithTests
        (AOC.BlockSolver (solve isInvalid2)) [("test1", "4174379265")]
    ]
  })

solve c = process c . parse

parse :: String -> [(Int,Int)]
parse = map yoink . splitOn ","
  where
    yoink t = let [a,b] = splitOn "-" t in (read a, read b)

process :: (Int -> Bool) -> [(Int,Int)] -> String
process c = show . sum . concatMap (findInvalid c)

findInvalid :: (Int -> Bool) -> (Int,Int) -> [Int]
findInvalid c (a,b) = filter c [a..b]

isInvalid1 :: Int -> Bool
isInvalid1 = check . show
  where
    check "" = False
    check a | odd (length a) = False
    check a = let (l,r) = splitAt (length a `div` 2) a in l == r

isInvalid2 :: Int -> Bool
isInvalid2 a = let
  digs = show a
  len = length digs
  lendivs = divisors len
  repl n = concat . take n . repeat
  check n = repl (div len n) (take n digs) == digs
 in
  any check lendivs

divisors :: Int -> [Int]
divisors a = filter ((0 ==) . mod a) [1..div a 2]
