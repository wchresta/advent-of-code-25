{-# language LambdaCase #-}
module Main where

import AOC
import Data.List (uncons, nub, singleton, foldl1')
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.Maybe (fromJust)

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=7
  , solvers=
    [ WithTests
        (AOC.BlockSolver solve)
        [("test1", "21")]
    , WithTests
        (AOC.BlockSolver (solve2 $ id))
        [("test1", "40")]
    ]
  })

type Dedup = [(Int,Int)] -> [(Int,Int)]


solve :: String -> String
solve = process . parse
  where parse = toGridWithIgnore (\_ m -> m) '.' . lines

solve2 :: Dedup -> String -> String
solve2 dedup = process2 . parse
  where parse = toGridWithIgnore (\_ m -> m) '.' . lines


process :: M.Map (Int, Int) Char -> String
process m = let
   prisms :: [(Int, Int)]
   prisms = M.keys m

   startIdx :: (Int, Int)
   startIdx = fst . fromJust . uncons . filter ((0 ==) . fst) $ prisms

   down (i,j) = (i+1,j)
   leftRight (i,j) = [(i,j-1),(i,j+1)]

   doSplit :: (Int, Int) -> (Int, [(Int, Int)])
   doSplit k =
    if k `elem` prisms
      then (1, leftRight k)
      else (0, [k])

   propagate :: [(Int, Int)] -> (Int, [(Int, Int)])
   propagate state = let
       mapped :: [(Int, [(Int, Int)])]
       mapped = map (doSplit . down . down) state
     in ( sum . map fst $ mapped
        , nub . concatMap snd $ mapped
        )

   propagateAll :: [(Int, Int)] -> Int
   propagateAll s = case propagate s of
     (0, res) -> 0
     (n, res) -> n + propagateAll res
 in show . propagateAll $ [startIdx]


type El = ((Int, Int), Int)

process2 :: M.Map (Int, Int) Char -> String
process2 m = let
   prisms :: [(Int, Int)]
   prisms = M.keys m

   startIdx :: (Int, Int)
   startIdx = fst . fromJust . uncons . filter ((0 ==) . fst) $ prisms

   down (i,j) = (i+1,j)
   leftRight (i,j) = [(i,j-1),(i,j+1)]

   doSplit :: El -> [El]
   doSplit (k,n) =
    if k `elem` prisms
      then map (,n) $ leftRight k
      else [(k, n)]

   propagate :: [El] -> [El]
   propagate = let
       dedup  = M.toList . M.fromListWith (+)
     in dedup . concatMap (doSplit . (\(k,n) -> (down . down $ k, n)))

   score = sum . map snd

   propagateAll :: [El] -> [El]
   propagateAll s = case propagate s of
     res | score res == score s -> res
     res | otherwise              -> propagateAll res
 in show . score . propagateAll $ [(startIdx, 1)]


