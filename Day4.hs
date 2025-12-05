module Main where

import AOC
import qualified Data.Map.Strict as M
import Data.Function (fix)

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=4
  , solvers=
    [ WithTests
        (AOC.BlockSolver $ solve1 . parse)
        [("test1", "13")]
    , WithTests
        (AOC.BlockSolver $ solve2 . parse)
        [("test1", "43")]
    ]
  })


type Mat = M.Map (Int, Int) Int

parse :: String -> Mat
parse = M.filter (1==) . matrixToMap . map (map (\x -> if x == '@' then 1 else 0)) . lines

solve1 :: Mat -> String
solve1 = show . fst . removeRolls

solve2 :: Mat -> String
solve2 m = show . fst . removeAllRolls $ m

removeAllRolls :: Mat -> (Int,Mat)
removeAllRolls m = removeAllRolls' $ (0,m)
  where
    removeAllRolls' :: (Int, Mat) -> (Int, Mat)
    removeAllRolls' (n, m) =
      case removeRolls m of
       (0,_)   -> (n,m)
       (n',m') -> removeAllRolls' (n+n',m')

removeRolls :: Mat -> (Int, Mat)
removeRolls m = M.foldlWithKey f (0,m) $ m
  where
    f :: (Int, Mat) -> (Int, Int) -> Int -> (Int, Mat)
    f (n,m') ij c = if c == 1 && countBoxes ij < 4 then (n+1, M.delete ij m') else (n, m')

    countBoxes ij = length $ filter hasBox (adjIdx ij)

    adjIdx :: (Int, Int) -> [(Int, Int)]
    adjIdx (i,j) = [ (i+di,j+dj) | dj <- [-1,0,1], di <- [-1,0,1], (di,dj) /= (0,0) ]

    hasBox :: (Int, Int) -> Bool
    hasBox ij = case M.lookup ij m of
        Just 1 -> True
        _      -> False


