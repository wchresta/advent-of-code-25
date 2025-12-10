module Main where

import AOC
import Data.List (sort,foldl',group,uncons)
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Debug.Trace

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=8
  , solvers=
    [ WithTests
        (AOC.BlockSolver solve)
        [("test1", "40")]
    , WithTests
        (AOC.BlockSolver solve2)
        [("test1", "25272")]
    ]
  })

data Vec3 = Vec3 { _x :: Int, _y :: Int, _z :: Int }

vZero = Vec3 0 0 0

instance Num Vec3 where
  (+) (Vec3 a b c) (Vec3 x y z) = Vec3 (a+x) (b+y) (c+z)
  (-) (Vec3 a b c) (Vec3 x y z) = Vec3 (a-x) (b-y) (c-z)
  (*) (Vec3 a b c) (Vec3 x y z) = Vec3 (a*x) (b*y) (c*z)
  negate (Vec3 a b c) = Vec3 (negate a) (negate b) (negate c)
  abs (Vec3 a b c) = Vec3 (abs a) (abs b) (abs c)
  signum (Vec3 a b c) = Vec3 (signum a) (signum b) (signum c)
  fromInteger x = let x' = fromInteger x in Vec3 x' x' x'

instance Show Vec3 where
  show (Vec3 x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Eq Vec3 where
  (==) (Vec3 a b c) (Vec3 x y z) = a == x && b == y && c == z

instance Ord Vec3 where
  compare (Vec3 x y z) (Vec3 a b c) = bunk (compare x a) (compare y b) (compare z c)
    where
      bunk EQ EQ x = x
      bunk EQ x _ = x
      bunk x _ _ = x

vNorm :: Vec3 -> Double
vNorm (Vec3 a b c) = sqrt (fromInteger . toInteger $ a*a + b*b + c*c)

vDist :: Vec3 -> Vec3 -> Double
vDist a b = vNorm $ b-a

vDiv :: Vec3 -> Int -> Vec3
vDiv (Vec3 a b c) x = Vec3 (div a x) (div b x) (div c x)

readVec3 :: String -> Vec3
readVec3 s = let [a,b,c] = splitOn "," s in Vec3 (read a) (read b) (read c)

solve :: String -> String
solve s = let
  vecs = parse s
  numDists = if length vecs > 30 then 1000 else 10
 in process numDists vecs


solve2 :: String -> String
solve2  = process2 . parse

parse :: String -> [Vec3]
parse = map readVec3 . lines

dists :: [Vec3] -> [(Double, Vec3, Vec3)]
dists = sort . map (\(a,b) -> (vDist a b, a, b)) . pairs

process :: Int -> [Vec3] -> String
process numDists vs = let
    minDists = take numDists $ dists vs
    mark (maxN,marks) (_,a,b) = case M.lookup a marks of
      Just n  -> case M.lookup b marks of
        Just m  -> (maxN,replaceMark n m marks)
        Nothing -> (maxN,M.insert b n marks)
      Nothing -> case M.lookup b marks of
        Just n  -> (maxN,M.insert a n marks)
        Nothing -> (maxN+1,M.insert a maxN $ M.insert b maxN marks)
    replaceMark n m marks = M.map (\k -> if k == n then m else k) marks
    allMarks = snd . foldl' mark (0,M.empty) $ minDists
    markSizes = map length . group . sort . M.elems $ allMarks
  in show . product . take 3 .reverse . sort $ markSizes

process2 :: [Vec3] -> String
process2 vs = let
    mark (maxN,ab,marks) (_,a,b) = case M.lookup a marks of
      Just n  -> case M.lookup b marks of
        Just m  -> (maxN,ab,replaceMark n m marks)
        Nothing -> (maxN,(a,b),M.insert b n marks)
      Nothing -> case M.lookup b marks of
        Just n  -> (maxN,(a,b),M.insert a n marks)
        Nothing -> (maxN+1,(a,b),M.insert a maxN $ M.insert b maxN marks)
    replaceMark n m marks = M.map (\k -> if k == n then m else k) marks
    marks = scanl mark (0,(vZero, vZero),M.empty) . dists $ vs
    notFinished (_,_,m) = M.size m < length vs
    Just (lastMark,_) = uncons . dropWhile notFinished $ marks
    (_,(a,b),_) = lastMark
  in show $ _x a * _x b

