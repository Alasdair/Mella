module Lang.Term.Parser 
    ( parseTerm
    , assignVars
    ) where

import Control.Arrow ((<<<))

import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as T

import Lang.Error
import Lang.Term
import Lang.Term.HParser
import Lang.Term.Tokenizer

-- | The Happy parser returns a term with a structure that reflects the 
-- notation. `desugar' takes strips the sugar from such terms and
-- returns a standard Term data type.
-- Note that it does not assign bound variables.
desugar :: SugarTerm -> Term
desugar (SLam args expr) = foldr Lam (desugar expr) (map Tag args)
desugar (SApp f x) = App (desugar f) (desugar x)
desugar (SId ty t1 t2) = Id (desugar ty) (desugar t1) (desugar t2)
desugar SRefl = Refl
desugar (SJ a b c d e f) = J (desugar a) (desugar b) (desugar c) (desugar d) (desugar e) (desugar f)
desugar (SVar v) = Named v
desugar (SNatLiteral n) = undefined
desugar SNat = undefined
desugar (SAnnotate t ty) = Ann (desugar t) (desugar ty)
desugar (SSort s) = Sort s
desugar SMeta = Meta 0
desugar (STySig args s t) = foldr (\tag t -> (Pi tag (desugar s) t)) (desugar t) (map Tag args)

-- | parse a Term within the provided Ctx.
parseTerm :: Ctx -> Text -> Error Term
parseTerm ctx t = tokenize t >>= sugarParse >>= return . assignVars ctx . desugar

instance IsString Term where
    fromString = fromError . parseTerm emptyCtx . T.pack
