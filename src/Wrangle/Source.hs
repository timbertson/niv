{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Wrangle.Source where

import Control.Monad
import Control.Error.Safe (justErr)
import Control.Exception (Exception)
import Control.Applicative ((<|>))
import Data.Aeson hiding (eitherDecodeFileStrict)
import Data.Aeson.Types (modifyFailure, typeMismatch, Parser)
import Data.Hashable (Hashable)
import GHC.Exts (toList)
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

sourceKeyJSON = "source" :: T.Text
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
newtype Sha256 = Sha256 String deriving (Show, Eq, FromJSON, ToJSON)
newtype Version = Version String deriving (Show, Eq, FromJSON, ToJSON)

instance AsString Sha256 where
  asString (Sha256 s) = s

data GithubFetch = GithubFetch {
  ghOwner :: String,
  ghRepo :: String,
  ghRev :: String,
  ghDigest :: Sha256
} deriving (Show, Eq)

instance ToStringPairs GithubFetch where
  toStringPairs GithubFetch { ghOwner, ghRepo, ghRev, ghDigest } =
    [
      ("owner", ghOwner),
      ("repo", ghRepo),
      ("rev", ghRev),
      ("sha256", asString ghDigest)
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

data UrlFetch = UrlFetch {
  urlType :: UrlFetchType,
  url :: String,
  urlDigest :: Sha256
} deriving (Show, Eq)

instance ToStringPairs UrlFetch where
  toStringPairs UrlFetch { urlType = _urlType, url, urlDigest } =
    [
      -- TODO: deal with urlType?
      ("url", url),
      ("sha256", asString urlDigest)
    ]

data GitFetch = GitFetch {
  gitUrl :: String,
  gitRev :: String,
  gitDigest :: Sha256
} deriving (Show, Eq)

instance ToStringPairs GitFetch where
  toStringPairs GitFetch { gitUrl, gitRev, gitDigest } =
    [
      -- TODO: deal with urlType?
      ("url", gitUrl),
      ("rev", gitRev),
      ("sha256", asString gitDigest)
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

data GitLocalFetch = GitLocalFetch {
  glPath :: LocalPath,
  glRef :: String
} deriving (Show, Eq)

instance ToStringPairs GitLocalFetch where
  toStringPairs GitLocalFetch { glPath, glRef } =
    [
      ("path", asString glPath),
      ("ref", glRef)
    ]

data SourceSpec
  = Github GithubFetch
  | Url UrlFetch
  | Git GitFetch
  | GitLocal GitLocalFetch
  deriving (Show, Eq)

instance ToStringPairs SourceSpec where
  toStringPairs (Github f) = toStringPairs f
  toStringPairs (Url f) = toStringPairs f
  toStringPairs (Git f) = toStringPairs f
  toStringPairs (GitLocal f) = toStringPairs f

orElse (Just x) _ = x
orElse Nothing x = x

instance FromJSON SourceSpec where
  parseJSON =
    withArray "SourceSpec" $ \v ->
      case (toList v) of
        [fetcher, Object args] ->
          (buildUrl <$> (parseJSON fetcher :: Parser UrlFetchType) <*> url <*> digest) <|>
          case fetcher of
            "github" ->
              build <$> owner <*> repo <*> rev <*> digest where
                build ghOwner   ghRepo   ghRev   ghDigest = Github $ GithubFetch {
                      ghOwner,  ghRepo,  ghRev,  ghDigest }
            "git" -> build <$> url <*> rev <*> digest where
              build gitUrl gitRev gitDigest = Git $ GitFetch { gitUrl, gitRev, gitDigest }
            "git-local" ->
              build <$> path <*> ref where
                build glPath glRef = GitLocal $ GitLocalFetch { glPath, glRef }
            _ ->  invalid (Array v)
          where
            digest = args .: "sha256"
            rev = args .: "rev"
            ref = args .: "ref"
            owner = args .: "owner"
            repo = args .: "repo"
            url = args .: "url"
            path = args .: "path"
            buildUrl urlType url urlDigest = Url $ UrlFetch { urlType, url, urlDigest }

        _ -> invalid (Array v)
      where
        invalid v = modifyFailure ("parsing `source` failed, " ++)
          (typeMismatch "pair" v)

sourceSpecAttrs :: SourceSpec -> (String, [(String,String)])
sourceSpecAttrs (Github fetch) = ("github", toStringPairs fetch)
sourceSpecAttrs (Url fetch) = (asString $ urlType fetch, toStringPairs fetch)
sourceSpecAttrs (Git fetch) = ("git", toStringPairs fetch)
sourceSpecAttrs (GitLocal fetch) = ("git-local", toStringPairs fetch)

instance ToJSON SourceSpec where
  toJSON spec = toJSON $ (fetcher, HMap.fromList attrs) where
    (fetcher, attrs) = sourceSpecAttrs spec

stringMapOfJson :: Aeson.Object -> Parser (HMap.HashMap String String)
stringMapOfJson args = HMap.fromList <$> (mapM convertArg $ HMap.toList args)
  where
    convertArg :: (T.Text, Aeson.Value) -> Parser (String, String)
    convertArg (k, v) =
      (\v -> (T.unpack k, T.unpack v)) <$> parseJSON v
  
data PackageSpec = PackageSpec {
  source :: SourceSpec,
  version :: Maybe Version,
  attrs :: StringMap
} deriving Show

instance FromJSON PackageSpec where
  parseJSON = withObject "PackageSpec" $ \attrs -> do
    (source, attrs) <- attrs `extract` sourceKeyJSON
    (version, attrs) <- attrs `extractMaybe` versionKeyJSON
    build version <$> parseJSON source <*> stringMapOfJson attrs
    where
      extract obj key = pairWithout key obj <$> obj .: key
      extractMaybe obj key = pairWithout key obj <$> obj .:? key
      pairWithout key obj v = (v, HMap.delete key obj)
      build version source attrs = PackageSpec { source, version, attrs }

instance ToJSON PackageSpec where
  toJSON PackageSpec { attrs, source } =
    toJSON $ HMap.insert
      (T.unpack sourceKeyJSON) (toJSON source) (HMap.map toJSON attrs)

newtype Sources = Sources
  { unSources :: HMap.HashMap PackageName PackageSpec }
  deriving newtype (Show)

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

loadSources :: [SourceFile] -> IO Sources
loadSources sources = do
  -- TODO
  putStrLn $ "Loading sources: " ++ (show sources)
  loadSourceFile (head sources)


newtype NotFound = NotFound (String, [String])
instance Show NotFound where
  show (NotFound (key, keys)) =
    "(key " <> key <> " not found in " <> (show keys) <> ")"

instance Exception NotFound

lookup :: PackageName -> Sources -> Either NotFound PackageSpec
lookup pkg sources =
  justErr (NotFound (show pkg, asString <$> HMap.keys sourceMap)) $ HMap.lookup pkg sourceMap
  where
    sourceMap = unSources sources

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
encodePrettyString = TL.unpack . TLE.decodeUtf8 . encodePretty

encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile path json =
  L.writeFile tmpPath (encodePretty json) >>
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
    "Cannot use " <> path,
    "The sources file should be a JSON map with toplevel objects `sources` and `wrangle`.",
    detail
  ]
