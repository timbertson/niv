{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Wrangle.Cmd where

import Control.Applicative
import Control.Monad
import Control.Monad.Except (throwError)
-- import Control.Exception (throw, AssertionFailed(..))
-- import qualified Data.HashMap.Strict as HMap
import Data.Char (toUpper)
import Data.Maybe (listToMaybe, fromMaybe)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
-- import qualified Nix.Expr as N
import Wrangle.Util
import qualified Wrangle.Source as Source
import Wrangle.Source (PackageName(..))
import qualified Wrangle.Splice as Splice
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts

main :: IO ()
main = join $ Opts.execParser opts
  where
    opts = Opts.info (parseCommand <**> Opts.helper) $ mconcat desc
    desc =
      [ Opts.fullDesc
      , Opts.header "Nix-wrangle - source & dependency manager for Nix projects"
      ]

parseCommand :: Opts.Parser (IO ())
parseCommand = Opts.subparser (
    -- Opts.command "init" parseCmdInit <>
    Opts.command "add"  parseCmdAdd <>
    -- Opts.command "update"  parseCmdUpdate <>
    Opts.command "splice"  parseCmdSplice <>
    Opts.command "show"  parseCmdShow )

{-

Planned commands / terminology:

nix/default.nix: derivation base. Contains deps & build instructions but not src
default.nix: concrete derivation: bakes in version of nixpkgs, self & wrangle
nix/wrangle.json: public deps
nix/wrangle-local.json: local deps


Use cases:
 - nix-wrangle splice: splice `self` into derivation base, to be used upstream (i.e. in nixpkgs)
 - nix-wrangle init: generate default.nix
 - nix-wrangle show: dump current details
 - nix-wrangle show --update: compare current versions against "head" / latest releases
 - nix-wrangle rm name [--source local]
 - nix-wrangle add name --type github [--source public]
 - nix-wrangle update name [--source public] (if no args given, does an auto-update)

 -}

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

parseAdd :: Opts.Parser (Either String (PackageName, Source.PackageSpec))
parseAdd =
  build <$> parseName <*> parsePackageAttrs
  where
    parseName :: Opts.Parser (Maybe String)
    parseName = Opts.optional $ Opts.argument Opts.str (Opts.metavar "NAME")

    build :: (Maybe String) -> (Source.StringMap) -> Either String (PackageName, Source.PackageSpec)
    build name attrs =
      case lookup "type"  of
        Nothing -> buildGithub
        Just "github" -> buildGithub
        Just t -> throwError ("Unsupported type: " <> (show t))
      where
        lookup attr = HMap.lookup attr attrs
        buildGithub :: Either String (PackageName, Source.PackageSpec)
        buildGithub = build <$> identity
          where
            build (name, ghOwner, ghRepo) =
              (name, Source.PackageSpec {
                Source.sourceSpec = Source.Github Source.GithubSpec { Source.ghOwner, Source.ghRepo, Source.ghRef },
                -- TODO drop consumed attrs, otherwise they'll be double published
                Source.packageAttrs = attrs,
                Source.fetchAttrs = Source.emptyAttrs
              })
              where ghRef = (fromMaybe (Source.Template "master") ref)

            ref = Source.Template <$> lookup "ref"

            identity :: Either String (PackageName, String, String)
            identity = case (name, lookup "owner", lookup "repo") of
              (name, Just owner, Just repo) -> Right (PackageName $ fromMaybe repo name, owner, repo)
              -- (Just name, Just owner, Nothing) -> Right (PackageName name, owner, name)
              (Just name, Nothing, Nothing) -> case span (/= '/') name of
                (owner, '/':repo) -> Right (PackageName repo, owner, repo)
                _ -> throwError ("`" <> name <> "` doesn't look like a github repo")
              (Nothing, _, _) -> throwError "name or --owner/--repo required"
              (_, Nothing, Just _) -> throwError "--owner required when using --repo"
              (_, Just _, Nothing) -> throwError "--repo required when using --owner"

parsePackageAttrs :: Opts.Parser (Source.StringMap)
parsePackageAttrs = HMap.fromList <$> many parseAttribute where
  parseAttribute :: Opts.Parser (String, String)
  parseAttribute =
    Opts.option (Opts.maybeReader parseKeyVal)
      ( Opts.long "attr" <>
        Opts.short 'a' <>
        Opts.metavar "KEY=VAL" <>
        Opts.help "Set the package spec attribute <KEY> to <VAL>"
      ) <|> shortcutAttributes <|>
    (("type",) <$> Opts.strOption
      ( Opts.long "type" <>
        Opts.short 'T' <>
        Opts.metavar "TYPE" <>
        Opts.help "The source type. Valid options: github | git | file | url | git-local"
      ))

  -- Parse "key=val" into ("key", "val")
  parseKeyVal :: String -> Maybe (String, String)
  parseKeyVal str = case span (/= '=') str of
    (key, '=':val) -> Just (key, val)
    _ -> Nothing

  -- Shortcuts for known attributes
  shortcutAttributes :: Opts.Parser (String, String)
  shortcutAttributes = foldr (<|>) empty $ mkShortcutAttribute <$>
    [
      ("ref", "github / git / git-local"),
      ("owner", "github"),
      ("repo", "github"),
      ("url", "url / file / git"),
      ("path", "git-local"),
      ("version", "all")
    ]

  mkShortcutAttribute :: (String, String) -> Opts.Parser (String, String)
  mkShortcutAttribute (attr, types) =
    (attr,) <$> Opts.strOption
      ( Opts.long attr <>
        Opts.metavar (toUpper <$> attr) <>
        Opts.help
          (
            "Equivalent to --attr " <> attr <> "=" <> (toUpper <$> attr) <>
            ", used for source type " <> types
          )
      )

-------------------------------------------------------------------------------
-- Show
-------------------------------------------------------------------------------
parseCmdShow :: Opts.ParserInfo (IO ())
parseCmdShow =
  Opts.info
    ((cmdShow <$> parseCommon) <**>
      Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Show sources"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle info"
      ]

cmdShow :: CommonOpts -> IO ()
cmdShow opts =
  do
    sourceFiles <- Source.configuredSources $ sources opts
    sources <- Source.loadSources sourceFiles
    putStrLn $ Source.encodePrettyString sources

-------------------------------------------------------------------------------
-- Add
-------------------------------------------------------------------------------
parseCmdAdd :: Opts.ParserInfo (IO ())
parseCmdAdd =
  Opts.info
    ((cmdAdd <$> parseAdd <*> parseCommon) <**>
      Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Add sources"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle add"
      ]

cmdAdd :: Either String (PackageName, Source.PackageSpec) -> CommonOpts -> IO ()
cmdAdd addOpt opts =
  do
    (name, spec) <- liftEither addOpt
    putStrLn $ "Adding " <> show name <> " // " <> show spec
    extantSourceFile <- listToMaybe <$> (Source.configuredSources $ sources opts)
    let loadedSourceFile :: Maybe (IO (Source.SourceFile, Maybe Source.Sources)) = loadSource <$> extantSourceFile
    source :: (Source.SourceFile, Maybe Source.Sources) <- fromMaybe (return (Source.DefaultSource, Nothing)) loadedSourceFile
    let (sourceFile, inputSource) = source
    let baseSource = fromMaybe (Source.emptySources) inputSource
    Source.writeSourceFile sourceFile baseSource
    putStrLn $ "Updated" <> (Source.pathOfSource sourceFile)
  where
    loadSource :: Source.SourceFile -> IO (Source.SourceFile, Maybe Source.Sources)
    loadSource f = (,) f . Just  <$> Source.loadSourceFile f
      

-------------------------------------------------------------------------------
-- Splice
-------------------------------------------------------------------------------
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
      , Opts.progDesc "Splice `self` dependency into a derivation base (typically nix/default.nix)"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle Splice"
      ]

cmdSplice :: CommonOpts -> FilePath -> IO ()
cmdSplice opts path =
  do
    fileContents <- Splice.load path
    let expr = Splice.parse fileContents
    expr <- Splice.getExn expr
    -- putStrLn $ show $ expr
    sourceFiles <- Source.configuredSources $ sources opts
    sources <- Source.loadSources sourceFiles
    self <- liftEither $ Source.lookup (PackageName "self") sources
    let existingSrcSpans = Splice.extractSourceLocs expr
    srcSpan <- case existingSrcSpans of
      [single] -> return single
      other -> error $ "No single source found in " ++ (show other)
    let replacedText = Splice.replaceSourceLoc fileContents self srcSpan
    putStrLn (T.unpack replacedText)

