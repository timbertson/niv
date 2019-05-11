{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Wrangle.Cmd where

import Control.Applicative
import Control.Monad
import Wrangle.Source
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts

main :: IO ()
main = join $ Opts.execParser opts
  where
    opts = Opts.info (parseCommand <**> Opts.helper) $ mconcat desc
    desc =
      [ Opts.fullDesc
      , Opts.header "NIV - Version manager for Nix projects"
      ]

parseCommand :: Opts.Parser (IO ())
parseCommand = Opts.subparser (
    -- Opts.command "init" parseCmdInit <>
    -- Opts.command "add"  parseCmdAdd <>
    -- Opts.command "show"  parseCmdShow <>
    -- Opts.command "update"  parseCmdUpdate <>
    -- Opts.command "drop"  parseCmdDrop <>
    Opts.command "info"  parseCmdInfo )

newtype CommonOpts = CommonOpts {
  sources :: [SourceFile]
} deriving newtype Show

parseCommon :: Opts.Parser CommonOpts
parseCommon =
  setSources <$> parseSourceOptions
  where
    setSources s = CommonOpts { sources = s }
    parseSourceOptions = many $ NamedSource <$> Opts.strOption
      ( Opts.long "source" <>
        Opts.short 's' <>
        Opts.metavar "SOURCE.json" <>
        Opts.help "Specify wrangle.json file to operate on"
      )

-------------------------------------------------------------------------------
-- Info
-------------------------------------------------------------------------------
parseCmdInfo :: Opts.ParserInfo (IO ())
parseCmdInfo =
  Opts.info
    ((cmdInfo <$> parseCommon) <**>
      Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Info dependency"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv info"
      ]

cmdInfo :: CommonOpts -> IO ()
cmdInfo opts =
  do
    sourceFiles <- configuredSources $ sources opts
    putStrLn $ "Loading sources: " ++ (show sourceFiles)
    sources <- loadSources sourceFiles
    putStrLn $ encodePrettyString sources


-- commands TODO:
-- wrangle add name url [--type github|url|path|git-local|etc]
--    version=... nix=...
--    git|github|git-local: ref=...
--
-- some fields may be templated, but maybe only template var is `version`?
-- [version]
