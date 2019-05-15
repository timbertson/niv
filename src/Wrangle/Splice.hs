{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Wrangle.Splice where

import Wrangle.Util
import Data.List.NonEmpty (NonEmpty((:|)))
-- import System.IO.Unsafe
import Nix.Expr hiding (stripAnnotation)
import qualified Wrangle.Source as Source
import qualified Nix.Expr as Expr
import qualified Nix.Expr.Shorthands as Shorthands
import qualified Nix.Pretty as Pretty
import qualified Data.Text as T
import Data.Fix
import Nix.Parser (Result(..), parseNixFileLoc)

data Opts = Opts {
  input :: FilePath,
  output :: Maybe FilePath,
  depName :: String
}

load :: FilePath -> IO (Result NExprLoc)
load = parseNixFileLoc

getExn :: Result NExprLoc -> IO NExprLoc
getExn (Success x) = return x
getExn (Failure f) = abort $ show f

repr :: NExprLoc -> String
repr expr = show $ stripAnnotation expr

stripAnnotation = Expr.stripAnnotation
pretty = Pretty.prettyNix

nixOfFetch :: Source.SourceSpec -> (NExpr, NExpr)
nixOfFetch fetch = (Fix $ NSym $ T.pack fetcher, nixAttrs fetchAttrs)
  where
    (fetcher, fetchAttrs) = Source.sourceSpecAttrs fetch
    var :: String -> NAttrPath NExpr
    var s = (StaticKey (T.pack s)) :| []
    nixAttrs :: [(String, String)] -> NExpr
    nixAttrs items = Fix $ NSet $ map strBinding items
    strBinding :: (String, String) -> Binding NExpr
    strBinding (key, val) = NamedVar (var key) (Shorthands.mkStr (T.pack val)) nullPos

replaceSource :: NExpr -> Source.SourceSpec -> NExpr
replaceSource expr newSource =
  cata (mapAttrs (mapSourceBinding updateSourceValue)) expr where
    mapAttrs fn (NSet bindings) = Fix $ NSet $ fn bindings
    mapAttrs fn (NRecSet bindings) = Fix $ NRecSet $ fn bindings
    mapAttrs _ other = Fix other

    updateSourceValue :: Fix NExprF -> Fix NExprF
    -- expect sources to be applications of a function applied to an attrset
    -- (e.g. fetchFromGitHub { ... })
    updateSourceValue (Fix (NBinary NApp fn _args)) = Fix $ NBinary NApp fn fetcherArgs
    -- If it doesn't match the expected pattern, just replace it with something plausible
    updateSourceValue _ = Fix $ NBinary NApp fetcherFn fetcherArgs

    (fetcherFn, fetcherArgs) = nixOfFetch newSource

    mapSourceBinding :: (NExpr -> NExpr) -> [Binding NExpr] -> [Binding NExpr]
    mapSourceBinding fn bindings = map apply bindings where
      -- TODO: update `pos`?
      apply (NamedVar name@((StaticKey "src") :| []) expr pos) = (NamedVar name (fn expr) pos)
      apply other = other
