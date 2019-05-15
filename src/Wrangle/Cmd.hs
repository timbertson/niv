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
-- import Control.Exception (throw, AssertionFailed(..))
-- import qualified Data.HashMap.Strict as HMap
import qualified Wrangle.Source as Source
import qualified Wrangle.Splice as Splice
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
    Opts.command "splice"  parseCmdSplice <>
    Opts.command "info"  parseCmdInfo )

newtype CommonOpts = CommonOpts {
  sources :: [Source.SourceFile]
} deriving newtype Show

parseCommon :: Opts.Parser CommonOpts
parseCommon =
  setSources <$> parseSourceOptions
  where
    setSources s = CommonOpts { sources = s }
    parseSourceOptions = many $ Source.NamedSource <$> Opts.strOption
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
    sourceFiles <- Source.configuredSources $ sources opts
    sources <- Source.loadSources sourceFiles
    putStrLn $ Source.encodePrettyString sources

parseCmdSplice :: Opts.ParserInfo (IO ())
parseCmdSplice =
  Opts.info
    ((cmdSplice <$> parseCommon <*> parseNixPath) <**>
      Opts.helper) $
    mconcat desc
  where
    parseNixPath = Opts.argument Opts.str (Opts.metavar "NIX_FILE")
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Splice dependency"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  niv Splice"
      ]

cmdSplice :: CommonOpts -> FilePath -> IO ()
cmdSplice opts path =
  do
    expr <- Splice.load path
    expr <- Splice.getExn expr
    putStrLn $ show $ expr
    let simplified = Splice.stripAnnotation expr

    sourceFiles <- Source.configuredSources $ sources opts
    sources <- Source.loadSources sourceFiles
    let self = Source.lookup (Source.PackageName "self") sources
    -- self <- justice self
    let replaced = Splice.replaceSource simplified <$> (Source.source <$> self)
    let pretty = Splice.pretty <$> replaced
    putStrLn $ show $ pretty

-- commands TODO:
-- wrangle add name url [--type github|url|path|git-local|etc]
--    version=... nix=...
--    git|github|git-local: ref=...
--
-- some fields may be templated, but maybe only template var is `version`?
-- [version]
