{-# LANGUAGE LambdaCase #-}

module Wrangle.Util where

import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Environment as Env

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

debugLn :: String -> IO ()
debugLn = case unsafePerformIO (Env.lookupEnv "DEBUG") of
  (Just "true") -> putStrLn . ("[debug]: " <>)
  (Just _) -> noop
  Nothing -> noop
  where
    noop _ = return ()

infoLn :: String -> IO ()
infoLn = putStrLn

errorLn :: String -> IO ()
errorLn = putStrLn . ("[error]: " <>)

tap :: (a -> IO ()) -> IO a -> IO a
tap action x = x >>= (\x -> action x >> return x)
