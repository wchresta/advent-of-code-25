{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-x-partial #-}
module AOC where

import qualified System.IO
import qualified Data.Text as T
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Criterion.Measurement
import Data.List (find)
import Control.DeepSeq (deepseq)
import Data.Foldable (toList)

data Solver a b
 = LineSolver { processLine :: a -> b, collectLines :: [b] -> a }
 | BlockSolver { processBlock :: a -> a }
 | ParseSolver { parser :: P.Parser b, processParsed :: b -> a }

data SolverWithTests a b
 = WithTests { tSolver :: Solver a b
             , tTests :: [(String, a)]
             }

runStringSolver :: Solver String b -> String -> String
runStringSolver (LineSolver l c) = c . map l . lines
runStringSolver (BlockSolver p) = p
runStringSolver (ParseSolver p s) =
  \inp -> case P.parse p "input" inp of
            Left err -> error . show $ err
            Right x -> s x

toStringSolver :: Solver T.Text b -> Solver String b
toStringSolver (LineSolver l c) = LineSolver (l . T.pack) (T.unpack . c)
toStringSolver (BlockSolver p) = BlockSolver $ T.unpack . p . T.pack


data SolutionWithTests a b = SolutionWithTests
 { dayNum :: Int
 , solvers :: [SolverWithTests a b]
 }

parseSolver :: P.Parser b -> (b -> a) -> [(String, a)] -> SolverWithTests a b
parseSolver p f = WithTests (ParseSolver p f)

runSolverWithTests :: SolutionWithTests String a -> IO ()
runSolverWithTests s = do
  initializeTime
  let dataPath = "inputs/day" ++ show (dayNum s)
  let runTest (want,slvr,testFile) = do
        inp <- readFile (dataPath ++ "_" ++ testFile)
        t1 <- getCPUTime
        let got = runStringSolver slvr inp
        t2 <- deepseq got getCPUTime
        ok <- test want got
        putStrLn $ "    took " ++ show (t2-t1)
        pure ok

  allRes <- mapM runTest [ (want, slvr, testFile) | WithTests slvr tsts <- solvers s, (testFile, want) <- tsts ]
  if and allRes
    then do
      inp <- readFile $ dataPath
      results <- mapM (pure . (`runStringSolver` inp) . tSolver) (solvers s)
      putStrLn $ unlines results
    else putStrLn "Some tests failed"


test :: String -> String -> IO Bool
test want got = do
  putStr "Test "
  let ok = want == got
  if ok
    then putStrLn $ "succeeded:  got " ++ got
    else putStrLn $ "**failed**: got " ++ got ++ " but wanted " ++ want
  pure ok

---

toGrid :: Eq a => ((Int,Int) -> M.Map (Int,Int) a -> b) -> [[a]] -> b
toGrid mk as = mk (maxI,maxJ) . M.fromList . indexify $ as
  where
    maxI = length as - 1
    maxJ = length (head as) - 1

toGridWithIgnore :: Eq a => ((Int,Int) -> M.Map (Int,Int) a -> b) -> a -> [[a]] -> b
toGridWithIgnore mk ign as = mk (maxI,maxJ) . M.fromList . filter ((ign/=) . snd) . indexify $ as
  where
    maxI = length as - 1
    maxJ = length (head as) - 1

indexify :: [[a]] -> [((Int, Int), a)]
indexify as = zip [ divMod i n | i <- [0..] ] $ concat as
  where n = length . head $ as

matrixToMap :: [[a]] -> M.Map (Int, Int) a
matrixToMap = M.fromList . indexify

mapToMatrix :: M.Map (Int, Int) a -> (Int, Int) -> a -> [[a]]
mapToMatrix m (maxI, maxJ) a =
  [
    [ M.findWithDefault a (i,j) m | j <- [0..maxJ] ]
    | i <- [0..maxI]
  ]

inGrid :: (Int, Int) -> (Int, Int) -> Bool
inGrid (maxI, maxJ) (i,j) = i >= 0 && j >= 0 && i <= maxI && j <= maxJ

findMinMax :: M.Map (Int, Int) a -> ((Int, Int), (Int, Int))
findMinMax = foldl (\((minI, minJ), (maxI, maxJ)) (i,j) -> ((min i minI, min j minJ), (max i maxI, max j maxJ))) ((0,0),(0,0)) . M.keys

-- |Non-repeating pairs, excluding (x,x)
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs xs = p (drop 1 xs) xs
  where
    p _ [] = []
    p _ [_] = []
    p [] (_:bs) = p (drop 1 bs) bs
    p (a:as) bs@(b:_) = (a,b) : p as bs

copy :: Int -> a -> [a]
copy 0 _ = []
copy n a = a:copy (n-1) a

cycleN :: Int -> [a] -> [a]
cycleN 0 _ = []
cycleN n as = as ++ cycleN (n-1) as

explodeMap :: Foldable t => M.Map a (t b) -> [(a, b)]
explodeMap = concatMap (\(a,bs) -> map (a,) $ toList bs) . M.toList

count :: (a -> Bool) -> [a] -> Int
count f = foldr (\x acc -> acc + if f x then 1 else 0) 0

