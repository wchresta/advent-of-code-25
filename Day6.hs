module Main where

import AOC
import Data.List (foldl1', transpose)
import Data.Char (isDigit)
import Debug.Trace

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=6
  , solvers=
    [ WithTests
        (AOC.BlockSolver solve1)
        [("test1", "4277556")]
    , WithTests
        (AOC.BlockSolver solve2)
        [("test1", "3263827")]
    ]
  })

type Op = Int -> Int -> Int

op :: String -> Op
op "*" = (*)
op "+" = (+)

type Puz = ([[Int]], [Op])

parse1 :: String -> Puz
parse1 s = let
   eatWords :: [[Int]] -> [[String]] -> Puz
   eatWords nums [ops] = (nums, map op ops)
   eatWords nums (ws:wss) = eatWords (map read ws : nums) wss

  in eatWords [] . map words . lines $ s

solve1 = let
   calcCols :: Puz -> [Int]
   calcCols (as,ops) = foldl1' (calcRow ops) as

   calcRow :: [Op] -> [Int] -> [Int] -> [Int]
   calcRow ops res = map (\(op,a,b) -> op a b) . zip3 ops res
 in show . sum . calcCols . parse1

solve2 = let
  parse = reverse . filter ([]/=) . map (filter (' '/=)) . transpose . lines
  collect (res,[]) w = (res,[read w])
  collect (res,as) w
    | last w == '+' = ((sum $ read (init w) : as):res, [])
    | last w == '*' = ((product $ read (init w) : as):res, [])
    | otherwise     = (res,read w:as)
 in show . sum . fst . foldl' collect ([],[]) . parse

