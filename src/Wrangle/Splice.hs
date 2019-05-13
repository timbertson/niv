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
import qualified Nix.Expr as Expr
import qualified Nix.Pretty as Pretty
import Data.Fix
import Nix.Atoms
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

replaceSource :: NExpr -> NExpr
replaceSource expr =
  cata (mapAttrs (mapSourceBinding updateSourceValue)) expr where
    mapAttrs fn (NSet bindings) = Fix $ NSet $ fn bindings
    mapAttrs fn (NRecSet bindings) = Fix $ NRecSet $ fn bindings
    mapAttrs _ other = Fix other

    -- expect sources to be applications of a function applied to an attrset
    -- (e.g. fetchFromGitHub { ... })
    -- TODO: if it doesn't match, just replace it with something plausible?
    updateSourceValue :: Fix NExprF -> Fix NExprF
    updateSourceValue (Fix (NBinary NApp fn arg)) = (Fix $ NBinary NApp fn (cata (mapAttrs updateSourceAttrBindings) arg))
    updateSourceValue _ = (Fix $ NConstant NNull)

    updateSourceAttrBindings :: [Binding (Fix NExprF)] -> [Binding (Fix NExprF)]
    updateSourceAttrBindings _ = [NamedVar (StaticKey "TODO" :| []) (Fix $ NConstant NNull) nullPos]

    mapSourceBinding :: (NExpr -> NExpr) -> [Binding NExpr] -> [Binding NExpr]
    mapSourceBinding fn bindings = map apply bindings where
      -- TODO: update `pos`?
      apply (NamedVar name@((StaticKey "src") :| []) expr pos) = (NamedVar name (fn expr) pos)
      apply other = other

