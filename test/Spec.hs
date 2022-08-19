{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Commands
import Data.Functor.Foldable
import Data.List
import qualified Data.Map as Map
import Polysemy
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.State hiding (get)
import Polysemy.Writer
import Relude hiding (evalState, lines, unlines)
import Test.HUnit

type LineNumber = Natural

-- | Runs the command line without IO
-- Outputs a list of outputs, tagged w/ the line number of input that corresponds to that output
-- i.e. if `p a1` is the second lind of input entered, then `(2, <a1contents>)` will be
-- the corresponding entry in the result of this function
runCommandLine :: [String] -> [(LineNumber, String)]
runCommandLine inputs =
  commandLine
    & ( runInputList inputs -- handle input by getting inputs from given input list
          >>> evalState Map.empty -- initial state is empty state
          >>> runFail -- stop outputting when explicit failure is hit
          >>> runOutputList -- handle output commands by pushing onto a list
          >>> run -- get the output list
          >>> fst -- discard unwanted error value
          >>> numberPrompts -- determine which line number of input corresponds to each output
          >>> filter ((/= "> ") . snd) -- get rid of prompts from our output
          >>> filter ((/= "\n") . snd) -- get rid of newlines from our output
      )
  where
    numberPrompts =
      usingReader 0 . cataA \case
        Nil -> return []
        -- if we found a prompt, then increment the index of everything after
        -- and including that element of the output
        Cons "> " rest -> do
          i <- ask -- `ask` asks for calculated line number of current list element
          updated <- local (+ 1) rest -- calculate the remaining line numbers, incremented by 1
          return $ (i + 1, "> ") : updated
        Cons elem rest -> liftA2 (:) ((,elem) <$> ask) rest -- annotate elem with the current index and proceed with the rest without incrementing

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
        [ (2, "3+4*5*6-7+8"),
          (3, "Int 124"),
          (5, "0.4-100*3.8+7"),
          (6, "Floating (-372.6)"),
          (8, "False"),
          (10, "True"),
          (12, "Floating 496.6"),
          (14, "Floating 138085.56")
        ]
    )

main :: IO ()
main = void $ runTestTT test1
