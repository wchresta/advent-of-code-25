module Main where

import AOC

main = AOC.runSolverWithTests (AOC.SolutionWithTests
  { dayNum=2
  , solvers=
    [ WithTests
        (AOC.LineSolver process result)
        [("test2", "%RESULT%")]
    ]
  })

process :: String -> String
process = "Hi"

result = show

