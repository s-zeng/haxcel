{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands
import Data.List
import qualified Data.Map as Map
import Polysemy
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Polysemy.Writer
import Relude hiding (evalState, lines, unlines)
import Test.HUnit

-- | Runs the command line without IO
runCommandLine :: [String] -> [String]
runCommandLine inputs =
  commandLine
    & ( runInputList inputs -- handle input by getting inputs from given input list
          >>> evalState Map.empty -- initial state is empty state
          >>> runFail -- stop outputting when explicit failure is hit
          >>> runOutputList -- handle output commands by pushing onto a list
          >>> run -- get the output list
          >>> fst -- discard unwanted error value
          >>> filter (/= "> ") -- get rid of prompts from our output
          >>> filter (/= "\n") -- get rid of newlines from our output
      )

test1 =
  TestCase
    ( assertEqual
        "readme test"
        ( runCommandLine
            [ "e a1 3+4*5*6-7+8",
              "p a1",
              "v a1",
              "e a2 0.4-100*3.8+7",
              "p a2",
              "v a2",
              "e a3 equal(a1, a2)",
              "v a3",
              "e a4 if(a3, false, true)",
              "v a4",
              "e a5 if(a4, a1-a2, sum(a1, a2, a1*a2))",
              "v a5",
              "e a1 -372.6",
              "v a5"
            ]
        )
        [ "3+4*5*6-7+8",
          "Int 124",
          "0.4-100*3.8+7",
          "Floating (-372.6)",
          "False",
          "True",
          "Floating 496.6",
          "Floating 138085.56"
        ]
    )

main :: IO ()
main = void $ runTestTT test1
