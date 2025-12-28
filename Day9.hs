{-# OPTIONS_GHC -fno-warn-x-partial #-}
module Main where

import AOC
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.String as P

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=9
  , solvers=
    [ parseSolver parse solve1 [("test1", "50")]
    , parseSolver parse solve2 [("test1", "24")]
    ]
  })

type Vec2 = (Int, Int)
type Area = (Vec2, Vec2)

parse :: P.Parser [Vec2]
parse = ((,) <$> (numberP <* P.char ',') <*> numberP) `P.sepEndBy1` P.newline

numberP :: P.Parser Int
numberP = read <$> P.many1 P.digit

squareArea :: Vec2 -> Vec2 -> Int
squareArea (a,b) (x,y) = (x-a+1) * (y-b+1)

solve :: ([Vec2] -> (Vec2,Vec2) -> Bool) -> [Vec2] -> Int
solve f vs = maximum . map (uncurry squareArea) . filter (f vs) . areas $ vs

areas :: [Vec2] -> [Area]
areas = map orderArea . pairs
  where
    orderArea ((x,y),(a,b)) = ((min x a, min y b), (max x a, max y b))

solve1 :: [Vec2] -> String
solve1 = show . solve (const . const True)

solve2 :: [Vec2] -> String
solve2 = show . solve isValid

sortVec2 :: Vec2 -> Vec2
sortVec2 (a,b) = (min a b, max a b)

isValid :: [Vec2] -> Area -> Bool
isValid vs = let
    lines :: [(Vec2,Vec2)]
    lines = zip vs (drop 1 vs ++ [head vs])

    intersects :: Area -> (Vec2, Vec2) -> Bool
    intersects ((alx,aly),(ahx,ahy)) ((sx,sy),(tx,ty))
      | sx == tx = let -- Line is vertical
      {-
       - (alx,aly)
       - ###########I### ly
       - #          I  #
       - #          I  #
       - ###########I### (ahx,ahy), hy
       -}
          (ly,hy) = sortVec2 (sy, ty)
        in alx < sx && sx < ahx && aly < hy && ly < ahy
      | sy == ty = let -- Line is horizontal
          (lx,hx) = sortVec2 (sx, tx)
        in aly < sy && sy < ahy && alx < hx && lx < ahx
  in not . (\a -> any (intersects a) lines)

