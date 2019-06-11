{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Wrangle.Splice where

import Wrangle.Util
import Data.Maybe (fromMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))
import System.IO.Unsafe
import Nix.Expr hiding (stripAnnotation)
import qualified Wrangle.Source as Source
import qualified Nix.Expr as Expr
import qualified Nix.Expr.Shorthands as Shorthands
import qualified Nix.Pretty as Pretty
import qualified Text.Megaparsec as Megaparsec
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Prettyprint.Doc as Doc
import qualified Data.Text.Prettyprint.Doc.Render.Text as Doc
import Data.Fix
import Nix.Parser (Result(..), parseNixTextLoc)

data Opts = Opts {
  input :: FilePath,
  output :: Maybe FilePath,
  depName :: String
}

load :: FilePath -> IO T.Text
load = T.readFile

parse :: T.Text -> Result NExprLoc
parse = parseNixTextLoc

getExn :: Result NExprLoc -> IO NExprLoc
getExn (Success x) = return x
getExn (Failure f) = abort $ show f

repr :: NExprLoc -> String
repr expr = show $ stripAnnotation expr

stripAnnotation :: NExprLoc -> NExpr
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

-- replaceSource :: NExpr -> Source.SourceSpec -> NExpr
-- replaceSource expr newSource =
--   cata (mapAttrs (mapSourceBinding updateSourceValue)) expr where
--     mapAttrs fn (NSet bindings) = Fix $ NSet $ fn bindings
--     mapAttrs fn (NRecSet bindings) = Fix $ NRecSet $ fn bindings
--     mapAttrs _ other = Fix other
--
--     updateSourceValue :: Fix NExprF -> Fix NExprF
--     -- expect sources to be applications of a function applied to an attrset
--     -- (e.g. fetchFromGitHub { ... })
--     updateSourceValue (Fix (NBinary NApp fn _args)) = Fix $ NBinary NApp fn fetcherArgs
--     -- If it doesn't match the expected pattern, just replace it with something plausible
--     updateSourceValue _ = Fix $ NBinary NApp fetcherFn fetcherArgs
--
--     (fetcherFn, fetcherArgs) = nixOfFetch newSource
--
--     mapSourceBinding :: (NExpr -> NExpr) -> [Binding NExpr] -> [Binding NExpr]
--     mapSourceBinding fn bindings = map apply bindings where
--       -- TODO: update `pos`?
--       apply (NamedVar name@((StaticKey "src") :| []) expr pos) = (NamedVar name (fn expr) pos)
--       apply other = other

replaceSourceLoc :: T.Text -> Source.SourceSpec -> (Maybe (Fix NExprF), SrcSpan) -> T.Text
replaceSourceLoc orig src (originalFetcherFn, span) =
  T.unlines $
    (take (startLine-1) origLines)
      ++ [
        partialStart <>
        -- "<<<" <> (T.pack $ show $ spanBegin span),
        srcText
        -- (T.pack $ show $ spanEnd span) <> ">>>"
        <>
        partialEnd
      ] ++ (drop (endLine) origLines)
  where
    origLines = T.lines orig
    -- TODO megaparse must have some niceness for this...
    columnToIndex :: Int -> T.Text -> Int
    columnToIndex col text = doSomething 0 0 (T.unpack text)
      where
        -- TODO it seems ridiculous that I have to map pos -> index this awkwardly
        colWidth '\t' = unPos Megaparsec.defaultTabWidth
        colWidth _ = 1
        doSomething _pos index [] = index
        doSomething pos index (char: remainder) =
          if pos >= col then index else doSomething (pos + colWidth char) (index+1) remainder
    partialStart = T.take (columnToIndex (startCol-1) line) line where line = origLines !! (startLine - 1)
    partialEnd = T.drop (columnToIndex (endCol-1) line) line where line = origLines !! (endLine - 1)
    startLine = (unPos . sourceLine . spanBegin) span
    startCol = (unPos . sourceColumn . spanBegin) span
    endLine = (unPos . sourceLine . spanEnd) span
    endCol = (unPos . sourceColumn . spanEnd) span

    -- TODO: add appropriate indentation
    srcText = Doc.renderStrict $ Doc.layoutCompact $ Pretty.prettyNix $ Fix $
      NBinary NApp (fromMaybe (defaultFetcherFn) originalFetcherFn) fetcherArgs
    (defaultFetcherFn, fetcherArgs) = nixOfFetch src

extractSourceLocs :: Fix NExprLocF -> [(Maybe (Fix NExprF), SrcSpan)]
extractSourceLocs expr =
  foldMap extractSources (unFix expr) where
    dbg :: String -> a -> a
    dbg s x = unsafePerformIO (putStrLn s >> return x)
    extractSources :: Fix NExprLocF -> [(Maybe (Fix NExprF), SrcSpan)]
    extractSources expr =
      case value of
        (NSet bindings) -> dbg "NSet bindings" $ extractSourceBindings bindings
        (NRecSet bindings) -> dbg "NRecSet bindings" $ extractSourceBindings bindings
        other -> foldMap extractSources other -- dbg ("nothing interesting -- " ++ (show other)) []
      where
        node = (getCompose . unFix) expr
        value = annotated node

    extractSourceBindings :: [Binding (Fix NExprLocF)] -> [(Maybe (Fix NExprF), SrcSpan)]
    extractSourceBindings bindings = concatMap apply bindings where
      apply :: Binding (Fix NExprLocF) -> [(Maybe (Fix NExprF), SrcSpan)]
      apply (NamedVar ((StaticKey "src") :| []) value _pos) =
        (extractSourceFn value, sourcePos) : extractSourceLocs value
        where
          sourcePos = (annotation . getCompose . unFix) value
      apply _ = dbg "non-srv binding" []

    extractSourceFn :: Fix NExprLocF -> Maybe (Fix NExprF)
    extractSourceFn expr = case nexpr of
      (NBinary NApp fn _args) -> Just (Expr.stripAnnotation fn)
      _ -> Nothing
      where
        nexpr = annotated . getCompose . unFix $ expr


