module Wrangle.Util where

import System.Exit (exitFailure)

-- Awkward workaround for now knowing the type of a string literal
s :: String -> String
s = id

abort :: String -> IO a
abort msg = do
  putStrLn msg
  exitFailure

