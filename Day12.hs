module Main where

import AOC
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=12
  , solvers=
    [ WithTests
        (AOC.ParseSolver parse solve)
        [("test1", "1")]
    ]
  })

type Puzzles = ([Box], [Puzzle])
type Puzzle = ((Int, Int), [Int])
type Box = [String]

parse :: P.Parser Puzzles
parse = do
  boxes <- P.many1 (P.try boxP <* P.newline)
  puzzles <- P.many1 (puzzleP <* P.newline)
  pure (boxes, puzzles)

boxP :: P.Parser Box
boxP = do
  P.digit
  P.char ':'
  P.newline
  P.many1 (P.many1 (P.oneOf ".#") <* P.newline)

intP :: P.Parser Int
intP = read <$> P.many1 P.digit

puzzleP :: P.Parser Puzzle
puzzleP = (,) <$> (dimP <* P.string ": ") <*> intP `P.sepBy1` P.char ' '

dimP :: P.Parser (Int, Int)
dimP = (,) <$> intP <* P.char 'x' <*> intP

solve :: Puzzles -> String
solve (bs, pzs) = show . length . filter (Just True ==) $ map (solvePuzzle (map boxFill bs)) pzs

solvePuzzle :: [Int] -> Puzzle -> Maybe Bool
solvePuzzle bs ((w,h),cs) = let
   minFill = sum . map (uncurry (*)) $ zip bs cs
   maxOutlineNeeded = sum cs * 9
   haveSpace = w * h
 in
   if minFill > haveSpace then Just False
   else if haveSpace >= maxOutlineNeeded then Just True
   else Nothing

boxFill :: Box -> Int
boxFill = length . filter ('#'==) . concat

