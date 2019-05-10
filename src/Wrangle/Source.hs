{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Wrangle.Source where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey, withObject, Value(..), parseJSON, (.:), withArray, toJSON)
import Data.Aeson.Types (modifyFailure, typeMismatch, Parser)
import Data.Char (toUpper)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.String.QQ (s)
import GHC.Exts (toList)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO.Unsafe (unsafePerformIO) -- tmp
import System.Process (readProcess)
import Wrangle.Util
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Help.Pretty as Opts
import qualified System.Directory as Dir

latestApiVersion = 1

type StringMap = HMap.HashMap String String

data FetchType
  = Github
  | Url
  | Git
  | GitLocal
  | Path
  | Unknown String
  deriving (Show, Eq)
instance FromJSON FetchType where
  parseJSON (String f) = pure $ case T.unpack f of
    "gihub" -> Github
    "url" -> Url
    "git-local" -> GitLocal
    "path" -> Path
    other -> Unknown other
  parseJSON other = typeMismatch "string" other

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

data PackageSpec' = PackageSpec' {
  attrs :: StringMap,
  source :: SourceSpec
} deriving Show

instance FromJSON PackageSpec' where
  parseJSON = withObject "PackageSpec" $ \obj ->
    build <$>
      (obj .: "source" >>= parseJSON) <*>
      (obj `without` "source" >>= stringMapOfJson)
    where
      without obj key = pure $ HMap.delete key obj
      build source attrs = PackageSpec' { source, attrs }

newtype Sources' = Sources'
  { unSources' :: HMap.HashMap PackageName PackageSpec' }
  deriving newtype Show

instance FromJSON Sources' where
  parseJSON = withObject "document" $ \obj ->
    (obj .: "wrangle" >>= checkHeader) >>
    Sources' <$> (obj .: "sources" >>= withObject "sources" parsePackageSpecs)
    where
      parsePackageSpecs attrs = HMap.fromList <$> mapM parseItem (HMap.toList attrs)
      parseItem :: (T.Text, Value) -> Parser (PackageName, PackageSpec')
      parseItem (k,v) = (PackageName $ T.unpack k,) <$> parseJSON v
      checkHeader = withObject "Wrangle Header" $ \obj ->
        (obj .: "apiversion" >>= checkApiVersion)
      checkApiVersion v =
        if v == (Number latestApiVersion)
          then pure ()
          else fail ("unsupported API version: " <> (TL.unpack . TLE.decodeUtf8 $ Aeson.encode v))

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

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString' and write it to a file.
encodeFile :: (ToJSON a) => FilePath -> a -> IO ()
encodeFile fp = L.writeFile fp . AesonPretty.encodePretty

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
