{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Wrangle.Source where

import Control.Monad
import Control.Exception
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, withObject, Value(..), parseJSON, (.:), withArray, toJSON)
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

latestApiVersion = 1

type StringMap = HMap.HashMap String String

sourceKeyJSON = "source" :: T.Text
sourcesKeyJSON = "sources" :: T.Text
wrangleKeyJSON = "wrangle" :: T.Text
apiversionKeyJSON = "apiversion" :: T.Text
wrangleHeaderJSON :: Aeson.Value
wrangleHeaderJSON =
  Object $ HMap.singleton apiversionKeyJSON (toJSON latestApiVersion)


data FetchType
  = Github
  | Url
  | Git
  | GitLocal
  | Path
  | Unknown String
  deriving (Show, Eq)

fetchTypeToStr = [
    (Github, "github"),
    (Url, "url"),
    (Git, "git"),
    (GitLocal, "git-local"),
    (Path, "path")
  ]

fetchTypeOfStr = map (\(a,b) -> (b,a)) fetchTypeToStr

orElse (Just x) _ = x
orElse Nothing x = x
  
instance FromJSON FetchType where
  parseJSON (String text) = pure $
    lookup str fetchTypeOfStr `orElse` Unknown str
    where str = T.unpack text

  parseJSON other = typeMismatch "string" other

instance ToJSON FetchType where
  toJSON typ = toJSON $ (lookup typ fetchTypeToStr) `orElse` (
    case typ of
      Unknown str -> str
      _ -> throw $ AssertionFailed "unknown fetch type, this is a big"
    )

stringMapOfJson :: Aeson.Object -> Parser (HMap.HashMap String String)
stringMapOfJson args = HMap.fromList <$> (mapM convertArg $ HMap.toList args)
  where
    convertArg :: (T.Text, Aeson.Value) -> Parser (String, String)
    convertArg (k, v) =
      (\v -> (T.unpack k, T.unpack v)) <$> parseJSON v
  
data SourceSpec = SourceSpec {
  fetcher :: FetchType,
  fetchArgs :: StringMap
} deriving Show
instance FromJSON SourceSpec where
  parseJSON =
    withArray "SourceSpec" $ \v ->
      case (toList v) of
        [fetcher, Object args] ->
          makeSpec <$> (parseJSON fetcher) <*> (stringMapOfJson args)
          where
            makeSpec fetcher fetchArgs = SourceSpec { fetcher, fetchArgs }
        _ -> invalid (Array v)
      where
        invalid v = modifyFailure ("parsing `source` failed, " ++)
          (typeMismatch "pair" v)

instance ToJSON SourceSpec where
  toJSON SourceSpec { fetcher, fetchArgs } = toJSON (fetcher, fetchArgs)

data PackageSpec' = PackageSpec' {
  attrs :: StringMap,
  source :: SourceSpec
} deriving Show

instance FromJSON PackageSpec' where
  parseJSON = withObject "PackageSpec" $ \obj ->
    build <$>
      (obj .: sourceKeyJSON >>= parseJSON) <*>
      (obj `without` sourceKeyJSON >>= stringMapOfJson)
    where
      without obj key = pure $ HMap.delete key obj
      build source attrs = PackageSpec' { source, attrs }

instance ToJSON PackageSpec' where
  toJSON PackageSpec' { attrs, source } =
    toJSON $ HMap.insert
      (T.unpack sourceKeyJSON) (toJSON source) (HMap.map toJSON attrs)

newtype Sources' = Sources'
  { unSources' :: HMap.HashMap PackageName PackageSpec' }
  deriving newtype (Show)

instance FromJSON Sources' where
  parseJSON = withObject "document" $ \obj ->
    (obj .: wrangleKeyJSON >>= checkHeader) >>
    Sources' <$> (obj .: sourcesKeyJSON >>= withObject "sources" parsePackageSpecs)
    where
      parsePackageSpecs attrs = HMap.fromList <$> mapM parseItem (HMap.toList attrs)
      parseItem :: (T.Text, Value) -> Parser (PackageName, PackageSpec')
      parseItem (k,v) = (PackageName $ T.unpack k,) <$> parseJSON v
      checkHeader = withObject "Wrangle Header" $ \obj ->
        (obj .: apiversionKeyJSON >>= checkApiVersion)
      checkApiVersion v =
        if v == (Number latestApiVersion)
          then pure ()
          else fail ("unsupported API version: " <> (TL.unpack . TLE.decodeUtf8 $ Aeson.encode v))

instance ToJSON Sources' where
  toJSON (Sources' s) = toJSON $
    HMap.fromList [
      (sourcesKeyJSON, toJSON s),
      (wrangleKeyJSON, wrangleHeaderJSON)
    ]

loadSourceFile :: SourceFile -> IO Sources'
loadSourceFile source = do
  -- -- TODO: if doesn't exist: run niv init
  putStrLn $ "Reading sources: " ++ sourcePath
  contents <- eitherDecodeFileStrict sourcePath
  case contents of
    Right source -> return source
    Left err -> abortInvalidSourceDocument sourcePath err
  where
    sourcePath = pathOfSource source

loadSources :: [SourceFile] -> IO Sources'
loadSources sources =
  -- TODO
  loadSourceFile (head sources)

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

parsePackageName :: Opts.Parser PackageName
parsePackageName = PackageName <$>
    Opts.argument Opts.str (Opts.metavar "PACKAGE")

-------------------------------------------------------------------------------
-- Aux
-------------------------------------------------------------------------------

--- Aeson

-- | Efficiently deserialize a JSON value from a file.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- The input file's content must consist solely of a JSON document,
-- with no trailing data except for whitespace.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
eitherDecodeFileStrict :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrict = fmap Aeson.eitherDecodeStrict . B.readFile

encodePretty :: (ToJSON a) => a -> L.ByteString
encodePretty = AesonPretty.encodePretty' (AesonPretty.defConfig {
  AesonPretty.confIndent = AesonPretty.Spaces 2,
  AesonPretty.confCompare = AesonPretty.compare
})

encodePrettyString :: (ToJSON a) => a -> String
encodePrettyString = TL.unpack . TLE.decodeUtf8 . encodePretty

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString' and write it to a file.
encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile fp = L.writeFile fp . encodePretty

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
