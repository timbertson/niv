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

import Prelude hiding (error)
import Control.Applicative
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Error.Safe (rightMay)
-- import Control.Exception (throw, AssertionFailed(..))
-- import qualified Data.HashMap.Strict as HMap
import Data.Char (toUpper)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (partition)
import Wrangle.Source (PackageName(..))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
-- import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as LB
-- import qualified Nix.Expr as N
import Wrangle.Util
import qualified Wrangle.Fetch as Fetch
import qualified Wrangle.Source as Source
import qualified System.Directory as Dir
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
    Opts.command "add" parseCmdAdd <>
    Opts.command "update" parseCmdUpdate <>
    Opts.command "splice" parseCmdSplice <>
    Opts.command "show" parseCmdShow <>
    Opts.command "default-nix" parseCmdDefaultNix
    )

{-

Planned commands / terminology:

nix/default.nix: derivation base. Contains deps & build instructions but not src
default.nix: concrete derivation: bakes in version of nixpkgs, self & wrangle
nix/wrangle.json: public deps
nix/wrangle-local.json: local deps


Use cases:
 - nix-wrangle splice: splice `self` into derivation base, to be used upstream (i.e. in nixpkgs)
 - nix-wrangle init: generate initial sources/wrangle.nix with wrangle and an optional pinned nixpkgs
 - nix-wrangle default-nix: generate default.nix
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

parseName :: Opts.Parser Source.PackageName
parseName = Source.PackageName <$> Opts.argument Opts.str (Opts.metavar "NAME")

parseNames :: Opts.Parser [Source.PackageName]
parseNames = many parseName

parseAdd :: Opts.Parser (Either AppError (PackageName, Source.PackageSpec))
parseAdd =
  mapLeft AppError <$> (build <$> Opts.optional parseName <*> parsePackageAttrs)
  where
    build :: (Maybe PackageName) -> (Source.StringMap) -> Either String (PackageName, Source.PackageSpec)
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
              (name, Just owner, Just repo) -> Right (fromMaybe (PackageName repo) name, owner, repo)
              -- (Just name, Just owner, Nothing) -> Right (PackageName name, owner, name)
              (Just (PackageName name), Nothing, Nothing) -> case span (/= '/') name of
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

cmdAdd :: Either AppError (PackageName, Source.PackageSpec) -> CommonOpts -> IO ()
cmdAdd addOpt opts =
  do
    (name, inputSpec) <- liftEither addOpt
    putStrLn $ "Adding " <> show name <> " // " <> show inputSpec
    extantSourceFile <- listToMaybe <$> (Source.configuredSources $ sources opts)
    let loadedSourceFile :: Maybe (IO (Source.SourceFile, Maybe Source.Sources)) = loadSource' <$> extantSourceFile
    source :: (Source.SourceFile, Maybe Source.Sources) <- fromMaybe (return (Source.DefaultSource, Nothing)) loadedSourceFile
    let (sourceFile, inputSource) = source
    let baseSource = fromMaybe (Source.emptySources) inputSource
    spec <- Fetch.prefetch name inputSpec
    let modifiedSource = Source.addSource baseSource name spec
    Source.writeSourceFile sourceFile modifiedSource
    putStrLn $ "Updated " <> (Source.pathOfSource sourceFile)
  where
    loadSource' :: Source.SourceFile -> IO (Source.SourceFile, Maybe Source.Sources)
    -- TODO: arrows?
    loadSource' f = (\(a,b) -> (a, Just b)) <$> loadSource f

loadSource :: Source.SourceFile -> IO (Source.SourceFile, Source.Sources)
loadSource f = (,) f <$> Source.loadSourceFile f
      
-------------------------------------------------------------------------------
-- Update
-------------------------------------------------------------------------------
parseCmdUpdate :: Opts.ParserInfo (IO ())
parseCmdUpdate =
  Opts.info
    ((cmdUpdate <$> Opts.optional parseNames <*> parsePackageAttrs <*> parseCommon) <**> Opts.helper) $
    mconcat desc
  where
    desc =
      [ Opts.fullDesc
      , Opts.progDesc "Update sources"
      , Opts.headerDoc $ Just $
          "Examples:" Opts.<$$>
          "" Opts.<$$>
          "  nix-wrangle update"
      ]

cmdUpdate :: Maybe [PackageName] -> Source.StringMap -> CommonOpts -> IO ()
cmdUpdate packageNamesOpt updateAttrs opts = do
  sourceFiles <- Source.configuredSources $ sources opts
  sources <- sequence $ map loadSource sourceFiles
  checkMissingKeys (map snd sources)
  sequence_ $ map updateSources sources
  where
    checkMissingKeys :: [Source.Sources] -> IO ()
    checkMissingKeys sources = case missingKeys of
      [] -> return ()
      _ -> fail $ "No such packages: " <> show missingKeys
      where
        (_, missingKeys) = partitionPackageNames $ Source.mergeSources sources

    partitionPackageNames :: Source.Sources -> ([PackageName], [PackageName])
    partitionPackageNames sources = case packageNamesOpt of
      Nothing -> (Source.keys sources, [])
      (Just names) -> partition (Source.member sources) names
    
    updateSingle :: Source.Sources -> PackageName -> IO Source.Sources
    updateSingle packages name = do
      infoLn $ " - updating " <> (show name) <> "..."
      original <- liftEither $ Source.lookup name packages
      newSpec <- liftEither $ Source.updatePackageSpec original updateAttrs
      fetched <- if newSpec /= original
        then Fetch.prefetch name newSpec
        else (infoLn "   ... unchanged" >> return newSpec)
      return $ Source.addSource packages name fetched

    updateSources :: (Source.SourceFile, Source.Sources) -> IO ()
    updateSources (sourceFile, sources) = do
      infoLn $ "Updating "<> Source.pathOfSource sourceFile <> " ..."
      let (packageNames, _) = partitionPackageNames sources
      updated <- foldM updateSingle sources packageNames
      Source.writeSourceFile sourceFile updated

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
    sources <- Source.mergeSources <$> Source.loadSources sourceFiles
    self <- liftEither $ Source.lookup (PackageName "self") sources
    let existingSrcSpans = Splice.extractSourceLocs expr
    srcSpan <- case existingSrcSpans of
      [single] -> return single
      other -> fail $ "No single source found in " ++ (show other)
    let replacedText = Splice.replaceSourceLoc fileContents self srcSpan
    putStrLn (T.unpack replacedText)


-------------------------------------------------------------------------------
-- default-nix
-------------------------------------------------------------------------------
parseCmdDefaultNix :: Opts.ParserInfo (IO ())
parseCmdDefaultNix =
  Opts.info
    ((cmdDefaultNix <$> parseCommon) <**>
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

cmdDefaultNix :: CommonOpts -> IO ()
cmdDefaultNix commonOpts = updateDefaultNix commonOpts (DefaultNixOpts { force = True })

data DefaultNixOpts = DefaultNixOpts {
  force :: Bool
}

updateDefaultNix :: CommonOpts -> DefaultNixOpts -> IO ()
updateDefaultNix commonOpts (DefaultNixOpts { force }) = do
  continue <- if force then return True else shouldWriteFile
  if continue then do
    infoLn $ "Updating " <> path
    sources <- (Source.mergeSources <$> (Source.loadSources . sources $ commonOpts))
    writeFile sources
  else do
    infoLn $ "Note: not replacing existing "<>path<>", run `nix-wrangle default-nix` to explicitly override"
  where
    path = "default.nix"
    markerText :: T.Text = "# This file is autogenerated by nix-wrangle"
    render :: Source.Sources -> Either AppError T.Text
    render sources = do
      let defaultPkgs = "import <nixpkgs>"
      nixpkgsExpr <- (fetchExpr defaultPkgs (PackageName "pkgs") sources) `orElse` (Right defaultPkgs)
      wrangleExpr <- (fetchExpr "pkgs" (PackageName "nix-wrangle") sources) `orElse` (Left $ AppError "no `nix-wrangle` entry found in sources")
      return $ T.unlines [
        markerText,
        "defaultPkgs = "<> nixpkgsExpr <> ";",
        "defaultWrangle = pkgs: { api = pkgs.callPackage \"${" <> wrangleExpr <> "}/nix/api.nix\"; };",
        "in",
        "{ pkgs ? defaultPkgs, nix-wrangle ? defaultWrangle pkgs }:",
        "(nix-wrangle.api { inherit pkgs; }).callPackage ./nix" ]

    fetchExpr :: T.Text -> PackageName -> Source.Sources -> Maybe (Either AppError T.Text)
    fetchExpr nixpkgsExpr name sources = expr <$> (rightMay $ Source.lookup name sources)
      where
        expr pkg = case (Source.sourceSpec pkg) of
          Source.Url _ -> Right $ "builtins.fetchTarball " <> (renderAttrs pkg)
          Source.Github _ -> Right $ nixpkgsExpr <> ".fetchFromGitHub " <> (renderAttrs pkg)
          other -> Left $ AppError $ "Unsupported fetch type `"<> (Source.nixName $ Source.fetcherName other) <>"` for bootstrap package `"<>(Source.unPackageName name)<>"`"

    renderAttrs :: Source.PackageSpec -> T.Text
    renderAttrs attrs = T.unlines [
      "{",
      T.unlines $ map (\(k,v) -> "  "<> (T.pack k) <> " = \"" <> (T.pack v) <> "\";") (HMap.toList . Source.fetchAttrs $ attrs),
      "}"]

    writeFile :: Source.Sources -> IO ()
    writeFile sources = do
      contents <- liftEither $ prefixAppError "Can't update default.nix:" $ render sources
      Source.writeFileContents path (LB.fromStrict . TE.encodeUtf8 $ contents)

    shouldWriteFile :: IO Bool
    shouldWriteFile = do
      exists <- Dir.doesFileExist path
      if exists then
        (T.isInfixOf markerText) <$> TE.decodeUtf8 <$> B.readFile path
      else
        return True

-- TODO:
-- - allow local overlays on global sources
--   e.g. prefetch against a local git repo but publish with the public URL
--
-- - init, and run auto-init / update whenever something changes
-- - during build, embed self.json so we can bootstrap this version? Or just use master.
-- - splice: specify output path
