{-# LANGUAGE NamedFieldPuns #-}

module Wrangle.Fetch where

-- import Control.Monad
-- import Control.Error.Safe (justErr)
-- import Control.Exception (Exception)
import Control.Monad.Except (throwError)
import Control.Applicative ((<|>), liftA2)
-- import Data.Aeson hiding (eitherDecodeFileStrict, encodeFile)
-- import Data.Aeson.Types (typeMismatch, Parser)
-- import Data.Hashable (Hashable)
-- import System.FilePath ((</>))
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Wrangle.Util
import Wrangle.Source
import qualified GitHub as GH
import qualified GitHub.Data.Name as GH
import qualified GitHub.Data.GitData as GHData
import qualified GitHub.Endpoints.GitData.References as GHRef
-- import qualified Data.Aeson as Aeson
-- import qualified Data.Aeson.Encode.Pretty as AesonPretty
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HMap
import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Encoding as TLE
-- import qualified Options.Applicative as Opts
-- import qualified System.Directory as Dir
-- import qualified System.FilePath.Posix as PosixPath

prefetch :: PackageSpec -> IO StringMap
prefetch pkg = do
  fetchAttrs <- HMap.fromList <$> resolveAttrs src
  sha256 <- prefetchSha256 (fetcherName src) fetchAttrs
  return $ HMap.insert "sha256" (asString sha256) fetchAttrs
  where
    src = sourceSpec pkg
    render = renderTemplate (packageAttrs pkg)

    resolveAttrs :: SourceSpec -> IO [(String,String)]
    resolveAttrs (Github (spec@ GithubSpec { ghOwner, ghRepo, ghRef })) = do
      ref <- liftEither $ render ghRef
      commit <- resolveGithubRef spec ref
      return [
        ("owner", ghOwner),
        ("repo", ghRepo),
        ("rev", (asString commit))]

    resolveAttrs (Url (UrlSpec { url })) = do
      url <- liftEither $ render url
      return [("url", url)]

    resolveAttrs (Git _) = error "TODO"

    resolveAttrs (GitLocal (GitLocalSpec { glPath, glRef })) = do
      ref <- liftEither $ render glRef
      return [("ref", ref), ("path", asString glPath)]

-- arbitrary prefetch plan:
-- invoke `nixpkgs.<fetcher> { args }`, setting sha256 to A{40}. Grep out a few patterns, falling bak to whichever 40-char string is != AAAAAA
-- https://github.com/seppeljordan/nix-prefetch-github/blob/cd9708fcdf033874451a879ac5fe68d7df930b7e/src/nix_prefetch_github/__init__.py#L124
-- Also note SRI: https://github.com/NixOS/nix/commit/6024dc1d97212130c19d3ff5ce6b1d102837eee6
-- and https://github.com/NixOS/nix/commit/5e6fa9092fb5be722f3568c687524416bc746423

resolveGithubRef :: GithubSpec -> String -> IO GitRevision
resolveGithubRef (GithubSpec { ghRepo, ghOwner }) ref =
  resolveGithubRef' ref <$> refs
  where
    refs = GHRef.references (name ghOwner) (name ghRepo) >>= liftEither
    name = GH.N . T.pack

resolveGithubRef' :: String -> Vector GH.GitReference -> GitRevision
resolveGithubRef' ref refs = extractCommit (
    nameEq ref <|> nameEq ("refs/tags/"<>ref) <|> nameEq ("refs/heads/"<>ref)
  )
  where
    nameEq :: String -> Maybe GH.GitReference
    nameEq candidate = Vector.find ((== T.pack candidate) . GHData.gitReferenceRef) refs
    extractCommit Nothing = GitRevision ref -- assume it's a commit
    extractCommit (Just ref) = GitRevision . T.unpack . GHData.gitObjectSha . GHData.gitReferenceObject $ ref

prefetchSha256 :: FetcherName -> StringMap -> IO Sha256
prefetchSha256 = error "TODO prefetchSha256"

renderTemplate :: StringMap -> Template -> Either String String
renderTemplate attrs fullText = render (asString fullText) where
  render :: String -> Either String String
  render ('<':str) = do
      case span (/= '>') str of
        (key, '>':rest) ->
          liftA2 (<>) value (render rest)
          where
            value = liftMaybe notFound $ HMap.lookup key attrs
            notFound = "Missing key `"<> key <>"` in template: " <> (asString fullText)
        _ -> throwError $ "Value contains an unterminated key: " <> (asString fullText)
  render (c:str) = (c:) <$> render str
  render [] = Right []
