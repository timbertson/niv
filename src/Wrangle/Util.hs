module Wrangle.Util where

import System.Exit (exitFailure)

-- Awkward workaround for not knowing the type of a string literal
s :: String -> String
s = id

abort :: String -> IO a
abort msg = do
  putStrLn msg
  exitFailure

liftEither (Left msg) = error (show msg)
liftEither (Right x) = return x

liftMaybe err Nothing = error err
liftMaybe _ (Just x) = return x

orElse (Just x) _ = x
orElse Nothing x = x

orEither (Right x) _ = x
orEither (Left _) dfl = dfl
