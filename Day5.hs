module Main where

import AOC
import Data.List (findIndex, uncons)
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=5
  , solvers=
    [ WithTests
        (AOC.ParseSolver parse solve1)
        [("test1", "3")]
    , WithTests
        (AOC.ParseSolver parse solve2)
        [("test1", "14")]
    ]
  })

type Puz = ([(Int,Int)],[Int])

parse :: P.Parser Puz
parse = do
  rs <- P.many1 (rangeP <* P.newline)
  P.newline
  ids <- P.many1 (idP <* P.newline)
  P.eof
  pure $ (rs, ids)

rangeP :: P.Parser (Int, Int)
rangeP = (,) <$> idP <*> (P.char '-' *> idP)

idP :: P.Parser Int
idP = numberP

numberP :: P.Parser Int
numberP = read <$> P.many1 P.digit

solve1 :: Puz -> String
solve1 (rgs,ns) = let
    inAnyRange n = any (\(a,b) -> a <= n && n <= b) rgs
  in
    show . length . filter inAnyRange $ ns

solve2 :: Puz -> String
solve2 = let
    overlaps (a,b) (x,y) = x <= a && a <= y || a <= x && x <= b

    unify rgs ab@(a,b) = case findIndex (overlaps ab) rgs of
      Just i -> let
          (left,right) = splitAt i rgs
          Just ((x,y), rst) = uncons right
          xy' = (min a x, max b y)
        in left ++ unify rst xy'
      Nothing -> ab : rgs

    rngLen (a,b) = b - a + 1

  in show . sum . map rngLen . foldl' unify [] . fst


