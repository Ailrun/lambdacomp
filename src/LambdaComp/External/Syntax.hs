{-# LANGUAGE PatternSynonyms #-}
module LambdaComp.External.Syntax
  ( module LambdaComp.External.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp

  , SourcePos (..)
  , sourcePosPretty
  ) where

import Data.String     (IsString (..))
import Text.Megaparsec (SourcePos (..), mkPos, sourcePosPretty)

import LambdaComp.Ident
import LambdaComp.PrimOp            (PrimOp (..), PrimOpArity (..))

data SourceSpan where
  SourceSpan :: { startPos :: SourcePos, endPos :: SourcePos } -> SourceSpan
  deriving stock (Eq, Ord, Show)

type Program = [XTop]

data Top where
  TopTmDecl :: { tmDefName :: Ident, tmDefType :: XTp } -> Top
  TopTmDef :: { tmDefName :: Ident, tmDefBody :: XTm } -> Top
  deriving stock (Eq, Ord, Show)

type XTop = (Top, SourceSpan)

data TpConst where
  TpCUnit   :: TpConst
  TpCBool   :: TpConst
  TpCInt    :: TpConst
  TpCDouble :: TpConst
  deriving stock (Eq, Ord, Show)

data Tp where
  TpConst  :: !TpConst -> Tp
  (:->:)   :: ![Tp] -> Tp -> Tp
  deriving stock (Eq, Ord, Show)
infixr 8 :->:

pattern TpFun :: [Tp] -> Tp -> Tp
pattern TpFun tpPs tpR = tpPs :->: tpR
{-# COMPLETE TpConst, TpFun #-}

type XTp = (Tp, SourceSpan)

data Param where
  Param :: { paramName :: !Ident, paramType :: !(Maybe Tp) } -> Param
  deriving stock (Eq, Ord, Show)

type XParam = (Param, SourceSpan)

data TmConst where
  TmCUnit        :: TmConst
  TmCTrue        :: TmConst
  TmCFalse       :: TmConst
  TmCInt         :: !Int -> TmConst
  TmCDouble      :: !Double -> TmConst
  deriving stock (Eq, Ord, Show)

data Tm where
  TmAnn         :: XTm -> XTp -> Tm
  TmVar         :: !Ident -> Tm
  TmConst       :: !TmConst -> Tm
  TmIf          :: XTm -> XTm -> XTm -> Tm
  TmLam         :: ![XParam] -> XTm -> Tm
  TmApp         :: XTm -> ![XTm] -> Tm
  TmPrimBinOp   :: !(PrimOp Binary) -> XTm -> XTm -> Tm
  TmPrimUnOp    :: !(PrimOp Unary) -> XTm -> Tm
  TmPrintInt    :: XTm -> XTm -> Tm
  TmPrintDouble :: XTm -> XTm -> Tm
  deriving stock (Eq, Ord, Show)

instance IsString Tm where
  fromString = TmVar . fromString

type XTm = (Tm, SourceSpan)

liftX2 :: ((a, SourceSpan) -> (b, SourceSpan) -> c) -> (a, SourceSpan) -> (b, SourceSpan) -> (c, SourceSpan)
liftX2 f op0 op1 = (f op0 op1, mergeSourceSpan (snd op0) (snd op1))

ghostSourceSpan :: SourceSpan
ghostSourceSpan = SourceSpan ghostSourcePos ghostSourcePos

ghostSourcePos :: SourcePos
ghostSourcePos = SourcePos "" (mkPos 0) (mkPos 0)

getSourceSpan :: (a, SourceSpan) -> SourceSpan
getSourceSpan = snd

mergeSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
mergeSourceSpan p q = SourceSpan (startPos p) (endPos q)
