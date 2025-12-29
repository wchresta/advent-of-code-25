{-# LANGUAGE OverloadedStrings #-}
module Main where

import AOC
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.String as P

import Data.Scientific (toBoundedInteger)
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import Data.String (fromString)
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Map.Lazy as Map
import qualified Numeric.Optimization.MIP as MIP
import qualified Numeric.Optimization.MIP.Solver as MIP
import Numeric.Optimization.MIP ((.<=.),(.==.))

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=10
  , solvers=
    [ parseIOSolver parse solve [("test1", "33")]
    ]
  })

type Puzzle = ([[Int]], [Int])

parse :: P.Parser [Puzzle]
parse = puzzleP `P.sepEndBy1` P.newline
  where
    puzzleP :: P.Parser Puzzle
    puzzleP =
        P.many (P.oneOf "[.#] ") *>
        ((,)
         <$> intListP '(' ')' `P.sepEndBy1` P.space
         <*> intListP '{' '}')

    numberP :: P.Parser Int
    numberP = read <$> P.many1 P.digit

    intListP :: Char -> Char -> P.Parser [Int]
    intListP l r = P.between (P.char l) (P.char r) $ numberP `P.sepBy1` P.char ','

solve :: [Puzzle] -> IO String
solve = fmap (show . sum) . mapM solvePuzzle

solvePuzzle :: Puzzle -> IO Int
solvePuzzle (btns,volts) = do
  let n = length volts
      b = length btns

      zs = repeat 0 :: [Int]
      toVec v [] = v ++ take (n - length v) zs
      toVec v (i:is) = toVec (v ++ take (i - length v) zs ++ [1]) is
      vecBtns = map (toVec []) btns

      bVars = map (fromString . show) $ take b ['a'..]
      bExpr = map MIP.varExpr bVars
      exprBtns = map (\(b,x) -> map (\k -> if k == 1 then x else 0) b) $ zip vecBtns bExpr

      prob =
        MIP.def
        { MIP.objectiveFunction =
            MIP.def
            { MIP.objDir = MIP.OptMin
            , MIP.objExpr = sum bExpr
            }
        , MIP.constraints =
            [ sum bs .==. MIP.constExpr (fromIntegral . toInteger $ v) | (bs, v) <- zip (transpose exprBtns) volts ]
        , MIP.varDomains =
            Map.fromList
            [ (n, (MIP.IntegerVariable, (0, MIP.PosInf))) | n <- bVars ]
        }

  sol <- MIP.solve MIP.cbc MIP.def{ MIP.solveTimeLimit = Just 10.0 } prob
  pure . fromJust . toBoundedInteger . sum . MIP.solVariables $ sol

