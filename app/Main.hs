module Main where

import Commands
import Control.Exception
import qualified Data.Map as Map
import Polysemy
import Polysemy.Fail
import Polysemy.Input
import Polysemy.Output
import Polysemy.State
import Relude hiding (evalState)

main :: IO ()
main =
  commandLine -- `loop` from above goes through the following handlers:
    & ( runInputSem (embed $ Just . toString <$> getLine) -- handle input with the `getLine` function
          >>> runOutputSem (\str -> embed (putStr str) >> embed (hFlush stdout)) -- handle output with the `putStr` function, and make sure we flush stdout
          >>> evalState Map.empty -- initialize state with an empty map
          >>> failToEmbed -- translate explicit failures to crashes
          >>> runM -- run the IO actions!
      )
