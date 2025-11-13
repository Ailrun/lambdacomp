{-# OPTIONS_GHC -Wno-orphans #-}
module LambdaComp.CBPV.PrettyPrinter
  (
  ) where

import Data.Text          (Text)
import Data.Text          qualified as T
import Prettyprinter
import Prettyprinter.Util

import LambdaComp.CBPV.Syntax
import LambdaComp.CBPV.TypeCheck (TypeError (..))

instance {-# OVERLAPPING #-} Pretty Program where
  pretty = align . concatWith (surround (line <> ";;" <> line)) . fmap pretty

instance Pretty Top where
  pretty TopTmDef {..} = pretty tmDefName <+> "=" <> nest 2 (group $ line <> pretty tmDefBody)

instance Pretty (Tp c) where
  pretty = prettyTpPrec 0

prettyTpPrec :: Int -> Tp c -> Doc ann
prettyTpPrec _  (TpConst tpc)  = pretty tpc
prettyTpPrec pr (TpUp tp)      = group $ prefixOfPrec1 pr ("Up", tpUpPrec) (group . (line <>) . (`prettyTpPrec` tp))
prettyTpPrec pr (tpP :->: tpR) = group $ prettyTpFun pr [tpP] tpR
prettyTpPrec pr (TpDown tp)    = group $ prefixOfPrec1 pr ("Down", tpDownPrec) (group . (line <>) . (`prettyTpPrec` tp))

prettyTpFun :: Int -> [Tp Val] -> Tp Com -> Doc ann
prettyTpFun pr rtpPs (tpP :->: tpR) = prettyTpFun pr (tpP : rtpPs) tpR
prettyTpFun pr rtpPs tp             = prefixOfPrec0 pr (align (sep $ (<+> "->") . prettyTpPrec (tpFunPrec + 1) <$> reverse rtpPs), tpFunPrec) (group . (line <>) . (`prettyTpPrec` tp))

tpFunPrec :: Int
tpUpPrec :: Int
tpDownPrec :: Int

tpFunPrec = 0
tpUpPrec = 1
tpDownPrec = 1

instance Pretty TpConst where
  pretty TpCUnit   = "Unit"
  pretty TpCBool   = "Bool"
  pretty TpCInt    = "Int"
  pretty TpCDouble = "Double"

instance Pretty (Tm c) where
  pretty = prettyTmPrec 0

prettyTmPrec :: Int -> Tm c -> Doc ann
prettyTmPrec _  (TmVar x)                    = pretty x
prettyTmPrec _  (TmGlobal x)                 = "#" <> pretty x
prettyTmPrec _  (TmConst c)                  = pretty c
prettyTmPrec pr (TmThunk tm)                 = group $ prefixOfPrec2 pr ("thunk", tmThunkPrec) (group . (line <>) . (`prettyTmPrec` tm))
prettyTmPrec pr (TmIf tm0 tm1 tm2)           = group $ condParens (pr > tmIfPrec) $ align $ vsep $ fmap group ["if" <> line <> pretty tm0, "then" <> line <> prettyTmPrec (tmIfPrec + 1) tm1, "else" <> line <> prettyTmPrec (tmIfPrec + 1) tm2]
prettyTmPrec pr (TmLam (BTyped p tm))        = group $ prettyTmLam pr [p] tm
prettyTmPrec pr (tmf `TmApp` tma)            = group $ infixlOfPrec pr (`prettyTmPrec` tmf) (emptyDoc, tmAppPrec) (group . (line <>) . (`prettyTmPrec` tma))
prettyTmPrec pr (TmForce tm)                 = group $ prefixOfPrec2 pr ("force", tmForcePrec) (group . (line <>) . (`prettyTmPrec` tm))
prettyTmPrec pr (TmReturn tm)                = group $ prefixOfPrec2 pr ("return", tmReturnPrec) (group . (line <>) . (`prettyTmPrec` tm))
prettyTmPrec pr (TmTo tm0 (BUntyped x tm1))  = group $ condParens (pr > tmToPrec) $ align $ group (prettyTmPrec (tmToPrec + 1) tm0 <> line <> "to" <+> pretty x <+> "->") <> line <> group (prettyTmPrec 0 tm1)
prettyTmPrec pr (TmLet tm0 (BUntyped x tm1)) = group $ condParens (pr > tmLetPrec) $ align $ prettyTmLet [(x, tm0)] tm1
prettyTmPrec pr (TmPrimBinOp op tm0 tm1)     = group $ prettyTmPrimBinOp pr op tm0 tm1
prettyTmPrec pr (TmPrimUnOp op tm)           = group $ prettyTmPrimUnOp pr op tm
prettyTmPrec pr (TmPrintInt tm0 tm1)         = condParens (pr > tmPrintPrec) $ align $ vsep ["printInt" <+> pretty tm0 <+> "then", pretty tm1]
prettyTmPrec pr (TmPrintDouble tm0 tm1)      = condParens (pr > tmPrintPrec) $ align $ vsep ["printDouble" <+> pretty tm0 <+> "then", pretty tm1]
prettyTmPrec pr (TmRec (BTyped p tm))        = group $ prefixOfPrec0 pr ("rec" <+> pretty p <+> "->", tmRecPrec) (group . (line <>) . (`prettyTmPrec` tm))

prettyTmLam :: Int -> [Param] -> Tm Com -> Doc ann
prettyTmLam pr rps (TmLam (BTyped p tm)) = prettyTmLam pr (p : rps) tm
prettyTmLam pr rps tm                    = prefixOfPrec0 pr ("\\" <+> align (sep $ pretty <$> reverse rps) <+> "->", tmLamPrec) (group . (line <>) . (`prettyTmPrec` tm))

prettyTmLet :: [(Ident, Tm Val)] -> Tm Com -> Doc ann
prettyTmLet rbs (TmLet tm0 (BUntyped x tm1)) = prettyTmLet ((x, tm0) : rbs) tm1
prettyTmLet rbs tm                           = vsep ["let", indent 2 . concatWith (surround $ ";" <> "line") . fmap prettyBinding $ reverse rbs, "in", pretty tm]
  where
    prettyBinding :: (Ident, Tm Val) -> Doc ann
    prettyBinding (x, tm') = pretty x <+> "=" <> softline <> pretty tm'

prettyTmPrimBinOp :: Int -> PrimOp Binary -> Tm Val -> Tm Val -> Doc ann
prettyTmPrimBinOp pr op tm0 tm1 = go op
  where
    go PrimIAdd = gol "+" tmAdditivePrec
    go PrimISub = gol "-" tmAdditivePrec
    go PrimIMul = gol "*" tmMultiplicativePrec
    go PrimIDiv = gol "/" tmMultiplicativePrec
    go PrimIMod = gol "%" tmMultiplicativePrec
    go PrimIEq  = gon "=" tmComparativePrec
    go PrimINEq = gon "<>" tmComparativePrec
    go PrimILt  = gon "<" tmComparativePrec
    go PrimILe  = gon "<=" tmComparativePrec
    go PrimIGt  = gon ">" tmComparativePrec
    go PrimIGe  = gon ">=" tmComparativePrec
    go PrimDAdd = gol "+." tmComparativePrec
    go PrimDSub = gol "-." tmComparativePrec
    go PrimDMul = gol "*." tmComparativePrec
    go PrimDDiv = gol "/." tmComparativePrec
    go PrimDEq  = gol "=." tmComparativePrec
    go PrimDNEq = gon "<>." tmComparativePrec
    go PrimDLt  = gon "<." tmComparativePrec
    go PrimDLe  = gon "<=." tmComparativePrec
    go PrimDGt  = gon ">." tmComparativePrec
    go PrimDGe  = gon ">=." tmComparativePrec
    go PrimBAnd = gol "&&" tmAndPrec
    go PrimBOr  = gol "||" tmOrPrec

    gol opDoc opPrec = infixlOfPrec pr (`prettyTmPrec` tm0) (softline <> opDoc <> space, opPrec) (`prettyTmPrec` tm1)
    gon opDoc opPrec = infixnOfPrec pr (`prettyTmPrec` tm0) (softline <> opDoc <> space, opPrec) (`prettyTmPrec` tm1)

prettyTmPrimUnOp :: Int -> PrimOp Unary -> Tm Val -> Doc ann
prettyTmPrimUnOp pr op tm = go op
  where
    go PrimINeg = go1 "-" tmNegPrec
    go PrimIToD = go1' "intToDouble" tmAppPrec
    go PrimDNeg = go1 "-." tmNegPrec
    go PrimDToI = go1' "doubleToInt" tmAppPrec
    go PrimBNot = go1 "~" tmNotPrec

    go1 opDoc opPrec = prefixOfPrec1 pr (opDoc <> space, opPrec) (`prettyTmPrec` tm)
    go1' opDoc opPrec = prefixOfPrec1 pr (opDoc <> softline, opPrec) (`prettyTmPrec` tm)

tmIfPrec :: Int
tmLamPrec :: Int
tmLetPrec :: Int
tmPrintPrec :: Int
tmRecPrec :: Int
tmThunkPrec :: Int
tmForcePrec :: Int
tmReturnPrec :: Int
tmToPrec :: Int
tmAppPrec :: Int

tmIfPrec = 0
tmLamPrec = 0
tmLetPrec = 0
tmPrintPrec = 0
tmRecPrec = 0
tmThunkPrec = 6
tmForcePrec = 6
tmReturnPrec = 6
tmToPrec = 6
tmAppPrec = 7

tmOrPrec :: Int
tmAndPrec :: Int
tmComparativePrec :: Int
tmAdditivePrec :: Int
tmMultiplicativePrec :: Int
tmNotPrec :: Int
tmNegPrec :: Int

tmOrPrec = 1
tmAndPrec = 2
tmComparativePrec = 3
tmAdditivePrec = 4
tmMultiplicativePrec = 5
tmNotPrec = 6
tmNegPrec = 8

instance Pretty TmConst where
  pretty TmCUnit       = "()"
  pretty TmCTrue       = "True"
  pretty TmCFalse      = "False"
  pretty (TmCInt i)    = pretty i
  pretty (TmCDouble d) = pretty d

instance Pretty Param where
  pretty Param {..} = parens . align . nest 2 $ pretty paramName <+> group (":" <+> pretty paramType)

instance Pretty TypeError where
  pretty (NonIntLastTopDecl x) =
    align
    $ vsep
    [ align
      $ fillSep
      [ reflow "Last top-level definition"
      , dquotes $ pretty x
      , reflow "of the main module does not have the Int type"
      ]
    , prettyErrorNote
      [ "the last top-level declaration determines"
      , "exit code of the entire program."
      ]
    ]
  pretty (NotInScope x) =
    align
    $ fillSep
    [ reflow "Variable"
    , dquotes $ pretty x
    , reflow "is not in the scope of the usage"
    ]
  pretty (NotDefined x) =
    align
    $ vsep
    [ align
      $ fillSep
      [ reflow "No top-level definition"
      , dquotes $ pretty x
      , reflow "is given but used"
      ]
    , prettyErrorNote
      [ "A body of a top-level definition can only access"
      , "top-levels defined before that."
      ]
    ]
  pretty (TypeMismatch pos tpExp tpAct) =
    align
    $ vsep
    [ reflow $ pos <> " expects"
    , indent 2 $ pretty tpExp
    , reflow "but found"
    , indent 2 $ pretty tpAct
    ]
  pretty (BranchTypeMismatch tpTrue tpFalse) =
    align
    $ vsep
    [ reflow "Branches of if have different types. One is with"
    , indent 2 $ pretty tpTrue
    , reflow "but the other is with"
    , indent 2 $ pretty tpFalse
    ]
  pretty (InvalidConstType tpChk tp) =
    align
    $ vsep
    [ reflow "A constructor is checked against"
    , indent 2 $ pretty tpChk
    , reflow "but it should have"
    , indent 2 $ pretty tp
    ]
  pretty (NonFunType tpAct) =
    align
    $ vsep
    [ reflow "Application expects a function type but found"
    , indent 2 $ pretty tpAct
    ]
  pretty (NonUpType tpAct) =
    align
    $ vsep
    [ reflow "Force expects a Up type but found"
    , indent 2 $ pretty tpAct
    ]
  pretty (NonDownType pos tpAct) =
    align
    $ vsep
    [ reflow $ pos <> " expects a Down type but found"
    , indent 2 $ pretty tpAct
    ]
  pretty err@(OfTop _ _) = align $ prettyDecoratedErr err
  pretty err@(OfRec _ _) = align $ prettyDecoratedErr err

prettyDecoratedErr :: TypeError -> Doc ann
prettyDecoratedErr (OfTop topName err) = "In top-level definition" <+> pretty topName <+> ":" <> softline <> prettyDecoratedErr err
prettyDecoratedErr (OfRec recName err) = "In recursive expression" <+> pretty recName <+> ":" <> softline <> prettyDecoratedErr err
prettyDecoratedErr err                 = indent 2 $ pretty err

prettyErrorNote :: [Text] -> Doc ann
prettyErrorNote =
  ((line <> "Note:" <> softline) <>)
  . align
  . reflow
  . T.intercalate " "

------------------------------------------------------------
-- Utility functions
------------------------------------------------------------

prefixOfPrec0 :: Int -> (Doc ann, Int) -> (Int -> Doc ann) -> Doc ann
prefixOfPrec0 pr (pre, prePrec) op = condParens (pr > prePrec) $ align $ pre <> nest 2 (op prePrec)

prefixOfPrec1 :: Int -> (Doc ann, Int) -> (Int -> Doc ann) -> Doc ann
prefixOfPrec1 pr (pre, prePrec) op = condParens (pr > prePrec) $ align $ pre <> nest 2 (op $ prePrec + 1)

prefixOfPrec2 :: Int -> (Doc ann, Int) -> (Int -> Doc ann) -> Doc ann
prefixOfPrec2 pr (pre, prePrec) op = condParens (pr > prePrec) $ align $ pre <> nest 2 (op $ prePrec + 2)

infixlOfPrec :: Int -> (Int -> Doc ann) -> (Doc ann, Int) -> (Int -> Doc ann) -> Doc ann
infixlOfPrec pr op0 (inl, inlPrec) op1 = condParens (pr > inlPrec) $ align $ op0 inlPrec <> nest 2 (inl <> op1 (inlPrec + 1))

infixnOfPrec :: Int -> (Int -> Doc ann) -> (Doc ann, Int) -> (Int -> Doc ann) -> Doc ann
infixnOfPrec pr op0 (inl, inlPrec) op1 = condParens (pr > inlPrec) $ align $ op0 (inlPrec + 1) <> nest 2 (inl <> op1 (inlPrec + 1))

condParens :: Bool -> Doc ann -> Doc ann
condParens True  = parens
condParens False = id
