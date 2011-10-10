{-# LANGUAGE OverloadedStrings #-}
module Lang.Util.WaldmeisterInput.ToProveIt where

import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tree

import Lang.Error
import Lang.PrettyPrint
import Lang.Util.WaldmeisterInput.HParser
import Lang.Util.WaldmeisterInput.Parser

convExpr :: Tree Text -> Text
convExpr (Node t []) = t
convExpr (Node t xs) =
    T.concat [ "(", t, " "
             , T.concat (intersperse " " (map convExpr xs))
             , ")" ]

inferExpr :: WMFile -> Tree Text -> Text
inferExpr file (Node sym _) =
  case (last <$> lookup sym (wmSig file)) of
      Just ty -> ty
      Nothing -> fst . head $ filter (\(ty, vars) -> sym `elem` vars) (wmVariables file)

inferEquation :: WMFile -> (Tree Text, Tree Text) -> Text
inferEquation file (expr1, expr2) =
  if inferExpr file expr1 == inferExpr file expr2
  then inferExpr file expr1
  else error ("both sides of an equation must have the same type\n"
              ++ drawTree (fmap show expr1) ++ ":" ++ show (inferExpr file expr1) ++ "\nAND\n"
              ++ drawTree (fmap show expr2))

convEquation :: WMFile -> (Tree Text, Tree Text) -> Text
convEquation file eq@(expr1, expr2) =
    T.concat [  convVars equationVars, " -> "
             , "Id ", inferEquation file eq
             , " ", convExpr expr1
             , " ", convExpr expr2
             ]
  where equationVars = mergeVars (exprVarTypes file expr1) (exprVarTypes file expr2)

convAxiom padding n file eq =
    T.concat [ padding, " -> (axiom", T.pack (show n), " : "
             , convEquation file eq
             , ")\n"
             ]

convVars :: [(Text, [Text])] -> Text
convVars [] = ""
convVars vs = T.concat (intersperse " " (map convVars' vs))
  where
    convVars' (ty, syms) = T.concat ["(", T.concat (intersperse " " syms), " : ", ty, ")"]

mergeVars :: (Ord k, Eq a) => [(k, [a])] -> [(k, [a])] -> [(k, [a])]
mergeVars vs1 vs2 = filter (\(k, as) -> not (null as)) $ Map.toList $ Map.unionWith f (Map.fromList vs1) (Map.fromList vs2)
  where f xs ys = nub (xs ++ ys)

vars :: WMFile -> Tree Text -> [Text]
vars file expr = filter (isNothing . flip lookup (wmSig file)) $ flatten expr

exprVarTypes :: WMFile -> Tree Text -> [(Text, [Text])]
exprVarTypes file expr =
    map (\ty -> (ty, filter (\x -> varHasType file x ty) (vars file expr))) (wmSorts file)

varHasType :: WMFile -> Text -> Text -> Bool
varHasType file sym ty = maybe False id $ elem sym <$> lookup ty (wmVariables file)

convSorts :: WMFile -> Text
convSorts file = T.concat ["(", T.concat (intersperse " " (wmSorts file)), " : *)"]

convFun :: (Text, [Text]) -> Text
convFun (f, ty) = T.concat [f, " : ", T.concat (intersperse " -> " ty)]

convSig :: [(Text, [Text])] -> Text
convSig sig = T.concat ["(", T.concat (intersperse ") (" (map convFun sig)), ")"]

toProveIt :: Int -> WMFile -> Text
toProveIt timeout file = T.concat
    [ "theorem ", wmName file, " : \"", convSorts file, "\n"
    , padding, " -> ", convSig (wmSig file), "\n"
    , T.concat (map (\(n, eq) -> convAxiom padding n file eq) (zip [1..] (wmEquations file)))
    , padding, convEquation file (wmConclusion file), "\".\n"
    , "  intro ", T.concat (intersperse " " (wmSorts file)), " "
    , T.concat (intersperse " " (map fst (wmSig file))), " "
    , T.concat (intersperse " " axNames)
    , ".\n"
    , "  waldmeister :timeout ", T.pack (show timeout), " "
    , ":axioms ", T.concat (intersperse " " axNames), " "
    , ":signature ", T.concat (intersperse " " (wmOrdering file)), ".\n"
    , "  qed :auto-sorry.\n\n"
    , "defined ", wmName file, ".\nwdisplaystats.\n\n"
    ]
  where
    padding = T.map (const ' ') (T.append "theorem " (wmName file))

    axNames = map (\n -> T.append "ax" (T.pack (show n))) [1..(length (wmEquations file))]
