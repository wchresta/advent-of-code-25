module Main where

import AOC

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=1
  , solvers=
    [ WithTests
        (AOC.LineSolver process collect1)
        [("test1", "3")]
    , WithTests
        (AOC.LineSolver process collect2)
        [("test1", "6"), ("test2", "7")]
    ]
  })

process :: String -> Int
process ('L' : num) = negate $ read num
process ('R' : num) = read num

collect1 = show . fst . collect
collect2 = show . snd . collect

collect :: [Int] -> (Int,Int)
collect = fst . foldl collect' ((0,0),50)
  where
    collect' ((n1,n2),dial) a = ((n1 + dn1, n2 + dn2), newdial)
      where
        newraw = dial + a
        newdial = (newraw + 100) `mod` 100
        dn1 = if newdial == 0 then 1 else 0
        dn2 = (abs (newraw `div` 100))
                  - (if dial == 0 && a < 0 then 1 else 0)
                  + (if newdial == 0 && a < 0 then 1 else 0)

