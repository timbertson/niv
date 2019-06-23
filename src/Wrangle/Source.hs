{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Wrangle.Source where

import Prelude hiding (error)
import Control.Monad
import Control.Error.Safe (justErr)
import Control.Exception (Exception)
import Control.Applicative ((<|>))
import Data.Aeson hiding (eitherDecodeFileStrict, encodeFile)
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Hashable (Hashable)
-- import GHC.Exts (toList)
import System.FilePath ((</>))
import Wrangle.Util
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Options.Applicative as Opts
import qualified System.Directory as Dir
import qualified System.FilePath.Posix as PosixPath

latestApiVersion = 1

type StringMap = HMap.HashMap String String

fetchKeyJSON = "fetch" :: T.Text
typeKeyJSON = "type" :: T.Text
versionKeyJSON = "version" :: T.Text
sourcesKeyJSON = "sources" :: T.Text
wrangleKeyJSON = "wrangle" :: T.Text
apiversionKeyJSON = "apiversion" :: T.Text
wrangleHeaderJSON :: Aeson.Value
wrangleHeaderJSON =
  Object $ HMap.singleton apiversionKeyJSON (toJSON latestApiVersion)

class ToStringPairs t where
  toStringPairs :: t -> [(String, String)]
  toStringMap :: t -> StringMap
  toStringMap = HMap.fromList . toStringPairs

class AsString t where
  asString :: t -> String

newtype Template = Template String deriving (Show, Eq, FromJSON, ToJSON)

instance AsString Template where
  asString (Template s) = s

newtype GitRevision = GitRevision String deriving (Show, Eq, FromJSON, ToJSON)

instance AsString GitRevision where
  asString (GitRevision s) = s

newtype Sha256 = Sha256 String deriving (Show, Eq, FromJSON, ToJSON)

instance AsString Sha256 where
  asString (Sha256 s) = s

data FetcherName = FetcherName {
  nixName :: String,
  wrangleName :: String
} deriving (Show, Eq)

data GithubSpec = GithubSpec {
  ghOwner :: String,
  ghRepo :: String,
  ghRef :: Template
} deriving (Show, Eq)

instance ToStringPairs GithubSpec where
  toStringPairs GithubSpec { ghOwner, ghRepo, ghRef } =
    [
      ("owner", ghOwner),
      ("repo", ghRepo),
      ("ref", asString ghRef)
    ]

data UrlFetchType = FetchArchive | FetchFile deriving (Show, Eq)

instance AsString UrlFetchType where
  asString FetchArchive = "url"
  asString FetchFile = "file"

instance ToJSON UrlFetchType where
  toJSON = toJSON . asString

instance FromJSON UrlFetchType where
  parseJSON (String "file") = pure FetchFile
  parseJSON (String "url") = pure FetchArchive
  parseJSON v = typeMismatch "\"file\" or \"url\"" v

data UrlSpec = UrlSpec {
  urlType :: UrlFetchType,
  url :: Template
} deriving (Show, Eq)

instance ToStringPairs UrlSpec where
  toStringPairs UrlSpec { urlType = _urlType, url } =
    [ ("url", asString url) ]

data GitSpec = GitSpec {
  gitUrl :: String,
  gitRef :: Template
} deriving (Show, Eq)

instance ToStringPairs GitSpec where
  toStringPairs GitSpec { gitUrl, gitRef } =
    [
      -- TODO: deal with urlType?
      ("url", gitUrl),
      ("ref", asString gitRef)
    ]

data LocalPath
  = FullPath FilePath
  | RelativePath FilePath
   deriving (Show, Eq)

instance FromJSON LocalPath where
  parseJSON (String text) = pure $
    if PosixPath.isAbsolute p then FullPath p else RelativePath p
    where p = T.unpack text
  parseJSON other = typeMismatch "string" other

instance AsString LocalPath where
  asString (FullPath s) = s
  asString (RelativePath s) = s

instance ToJSON LocalPath where
  toJSON p = toJSON (asString p)

data GitLocalSpec = GitLocalSpec {
  glPath :: LocalPath,
  glRef :: Template
} deriving (Show, Eq)

instance ToStringPairs GitLocalSpec where
  toStringPairs GitLocalSpec { glPath, glRef } =
    [
      ("path", asString glPath),
      ("ref", asString glRef)
    ]

data SourceSpec
  = Github GithubSpec
  | Url UrlSpec
  | Git GitSpec
  | GitLocal GitLocalSpec
  deriving (Show, Eq)

instance ToStringPairs SourceSpec where
  toStringPairs (Github f) = toStringPairs f
  toStringPairs (Url f) = toStringPairs f
  toStringPairs (Git f) = toStringPairs f
  toStringPairs (GitLocal f) = toStringPairs f

-- instance FromJSON SourceSpec where
--   parseJSON = withObject "SourceSpec" $ parseSourceSpecObject
--
-- TODO return (SourceSpec, remainingAttrs)
parseSourceSpecObject :: Value -> Object -> Parser SourceSpec
parseSourceSpecObject fetcher attrs = parseUrl <|> parseExplicit
  where
    parseUrl = (buildUrl <$> (parseJSON fetcher :: Parser UrlFetchType) <*> url)
    parseExplicit = case fetcher of
      "github" ->
        build <$> owner <*> repo <*> ref where
          build ghOwner  ghRepo  ghRef = Github $ GithubSpec {
                ghOwner, ghRepo, ghRef }
      "git" -> build <$> url <*> ref where
        build gitUrl gitRef = Git $ GitSpec { gitUrl, gitRef }
      "git-local" ->
        build <$> path <*> ref where
          build glPath glRef = GitLocal $ GitLocalSpec { glPath, glRef }
      _ ->  invalid attrs
    owner = attrs .: "owner"
    repo = attrs .: "repo"
    url = attrs .: "url"
    path = attrs .: "path"
    ref :: Parser Template = attrs .: "ref"
    buildUrl urlType url = Url $ UrlSpec { urlType, url = Template url }
    invalid v = fail $ "Unable to parse SourceSpec from: " <> (encodePrettyString v)

fetcherName :: SourceSpec -> FetcherName
fetcherName spec = FetcherName { nixName, wrangleName } where
  (nixName, wrangleName) = case spec of
    (Github _) ->   ("fetchFromGitHub", "github")
    (Git _) ->      ("fetchgit", "git")
    (GitLocal _) -> ("exportGitLocal", "git-local")
    (Url fetch) ->  ("fetchurl", asString $ urlType fetch)

stringMapOfJson :: Aeson.Object -> Parser (HMap.HashMap String String)
stringMapOfJson args = HMap.fromList <$> (mapM convertArg $ HMap.toList args)
  where
    convertArg :: (T.Text, Aeson.Value) -> Parser (String, String)
    convertArg (k, v) =
      (\v -> (T.unpack k, T.unpack v)) <$> parseJSON v
  
data PackageSpec = PackageSpec {
  sourceSpec :: SourceSpec,
  fetchAttrs :: StringMap,
  packageAttrs :: StringMap
} deriving Show

instance FromJSON PackageSpec where
  parseJSON = withObject "PackageSpec" $ \attrs -> do
    (fetchJSON, attrs) <- attrs `extract` fetchKeyJSON
    (fetcher, attrs) <- attrs `extract` typeKeyJSON
    (fetchAttrs :: StringMap) <- parseJSON fetchJSON
    build
      <$> (parseSourceSpecObject fetcher attrs)
      <*> (pure fetchAttrs) <*> stringMapOfJson attrs
    where
      extract obj key = pairWithout key obj <$> obj .: key
      -- extractMaybe obj key = pairWithout key obj <$> obj .:? key
      pairWithout key obj v = (v, HMap.delete key obj)
      build sourceSpec fetchAttrs packageAttrs = PackageSpec {
        sourceSpec, fetchAttrs, packageAttrs
      }

instance ToJSON PackageSpec where
  toJSON PackageSpec { sourceSpec, fetchAttrs, packageAttrs } =
    toJSON
      . HMap.insert (T.unpack typeKeyJSON) (toJSON . wrangleName . fetcherName $ sourceSpec)
      . HMap.insert (T.unpack fetchKeyJSON) (toJSON fetchAttrs)
      $ (HMap.map toJSON (packageAttrs <> HMap.fromList (toStringPairs sourceSpec)))

newtype Sources = Sources
  { unSources :: HMap.HashMap PackageName PackageSpec }
  deriving newtype (Show)

emptySources = Sources { unSources = HMap.empty }

addSource :: Sources -> PackageName -> PackageSpec -> Sources
addSource s name spec = Sources $ HMap.insert name spec $ unSources s

emptyAttrs :: StringMap
emptyAttrs = HMap.empty

instance FromJSON Sources where
  parseJSON = withObject "document" $ \obj ->
    (obj .: wrangleKeyJSON >>= checkHeader) >>
    Sources <$> (obj .: sourcesKeyJSON >>= withObject "sources" parsePackageSpecs)
    where
      parsePackageSpecs attrs = HMap.fromList <$> mapM parseItem (HMap.toList attrs)
      parseItem :: (T.Text, Value) -> Parser (PackageName, PackageSpec)
      parseItem (k,v) = (PackageName $ T.unpack k,) <$> parseJSON v
      checkHeader = withObject "Wrangle Header" $ \obj ->
        (obj .: apiversionKeyJSON >>= checkApiVersion)
      checkApiVersion v =
        if v == (Number latestApiVersion)
          then pure ()
          else fail ("unsupported API version: " <> (TL.unpack . TLE.decodeUtf8 $ Aeson.encode v))

instance ToJSON Sources where
  toJSON (Sources s) = toJSON $
    HMap.fromList [
      (sourcesKeyJSON, toJSON s),
      (wrangleKeyJSON, wrangleHeaderJSON)
    ]

loadSourceFile :: SourceFile -> IO Sources
loadSourceFile source = do
  -- -- TODO: if doesn't exist: run niv init
  putStrLn $ "Reading sources: " ++ sourcePath
  contents <- eitherDecodeFileStrict sourcePath
  case contents of
    Right source -> return source
    Left err -> abortInvalidSourceDocument sourcePath err
  where
    sourcePath = pathOfSource source

loadSources :: [SourceFile] -> IO [Sources]
loadSources sources = do
  putStrLn $ "Loading sources: " ++ (show sources)
  traverse loadSourceFile sources

mergeSources :: [Sources] -> Sources
mergeSources sources = do
  -- TODO check order
  Sources $ foldr HMap.union HMap.empty (map unSources sources)

writeSourceFile :: SourceFile -> Sources -> IO ()
writeSourceFile sourceFile = encodeFile (pathOfSource sourceFile)

newtype NotFound = NotFound (String, [String])
instance Show NotFound where
  show (NotFound (key, keys)) =
    "key `" <> key <> "` not found in " <> (show keys)

instance Exception NotFound

lookup :: PackageName -> Sources -> Either NotFound PackageSpec
lookup pkg sources = justErr err $ HMap.lookup pkg packages where
  err = (NotFound (show pkg, asString <$> HMap.keys packages))
  packages = unSources sources

defaultSourceFileCandidates :: [SourceFile]
defaultSourceFileCandidates = [ DefaultSource, LocalSource ]

detectDefaultSources :: IO [SourceFile]
detectDefaultSources =
  filterM (Dir.doesFileExist . pathOfSource) defaultSourceFileCandidates

configuredSources :: [SourceFile] -> IO [SourceFile]
configuredSources [] = detectDefaultSources
configuredSources explicitSources = return explicitSources

newtype PackageName = PackageName { unPackageName :: String }
  deriving newtype (Eq, Hashable, FromJSONKey, ToJSONKey, Show)

instance AsString PackageName where asString = unPackageName

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

eitherDecodeFileStrict :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrict = fmap Aeson.eitherDecodeStrict . B.readFile

encodePretty :: (ToJSON a) => a -> L.ByteString
encodePretty = AesonPretty.encodePretty' (AesonPretty.defConfig {
  AesonPretty.confIndent = AesonPretty.Spaces 2,
  AesonPretty.confCompare = AesonPretty.compare
})

encodePrettyString :: (ToJSON a) => a -> String
encodePrettyString = stringOfLazy . encodePretty

encodeOnelineString :: (ToJSON a) => a -> String
encodeOnelineString = stringOfLazy . Aeson.encode

stringOfLazy :: L.ByteString -> String
stringOfLazy = TL.unpack . TLE.decodeUtf8

encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile path json = writeFileContents path (encodePretty json)

writeFileContents :: FilePath -> L.ByteString -> IO ()
writeFileContents path contents = do
  L.writeFile tmpPath contents
  Dir.renameFile tmpPath path
  where tmpPath = path <> ".tmp" :: FilePath

data SourceFile
  = DefaultSource
  | LocalSource
  | NamedSource String
  deriving Show

pathOfSource :: SourceFile -> FilePath
pathOfSource source = case source of
  DefaultSource -> "nix" </> "wrangle.json"
  LocalSource -> "nix" </> "wrangle-local.json"
  NamedSource path -> path

-------------------------------------------------------------------------------
-- Abort
-------------------------------------------------------------------------------

abortInvalidSourceDocument :: FilePath -> String -> IO a
abortInvalidSourceDocument path detail = abort $ unlines [
    "Cannot load " <> path,
    "The sources file should be a JSON map with toplevel objects `sources` and `wrangle`.",
    detail
  ]
