module Main where

import AOC
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.String as P
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad.Memo (Memo, memo, startEvalMemo)

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=11
  , solvers=
    [ parseSolver parse solve1 [("test1", "5")]
    , parseSolver parse solve2 [("test2", "2")]
    ]
  })

type Puzzle = [(String, [String])]

parse :: P.Parser Puzzle
parse = (connectionsP `P.sepEndBy1` P.newline) <* P.eof

connectionsP :: P.Parser (String, [String])
connectionsP = (,) <$> (deviceP <* P.string ": ") <*> (deviceP `P.sepBy1` P.char ' ')

deviceP :: P.Parser String
deviceP = P.many1 P.alphaNum P.<?> "device name"


solve1 :: Puzzle -> String
solve1 = show . sumCounter . flip countPaths "you" . M.fromList


solve2 :: Puzzle -> String
solve2 = show . bothCounter . flip countPaths "svr" . M.fromList

type Counter = (Int, Int, Int, Int)

sumCounter :: Counter -> Int
sumCounter (b,f,d,a) = b+f+d+a

bothCounter :: Counter -> Int
bothCounter (b,_,_,_) = b

countPaths :: M.Map String [String] -> String -> Counter
countPaths m = startEvalMemo . countPathsm m

countPathsm :: M.Map String [String] -> String -> Memo String Counter Counter
countPathsm _ "out" = pure (0,0,0,1)
countPathsm m k = let
    conns :: [String]
    conns = fromMaybe (error $ "Missing key " ++ show k) $ M.lookup k m

    inc :: String -> Counter -> Counter
    inc "fft" (b,f,d,a) = (b+d,f+a,0,0)
    inc "dac" (b,f,d,a) = (b+f,0,d+a,0)
    inc _ bs = bs

    unionCounter :: Counter -> Counter -> Counter
    unionCounter (b,f,d,a) (b',f',d',a') = (b+b',f+f',d+d',a+a')
  in do
    connsCounts <- mapM (memo $ countPathsm m) conns
    pure $ inc k $ foldl1 unionCounter connsCounts

