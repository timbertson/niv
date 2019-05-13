module Wrangle.Util where

import System.Exit (exitFailure)

abort :: String -> IO a
abort msg = do
  putStrLn msg
  exitFailure

