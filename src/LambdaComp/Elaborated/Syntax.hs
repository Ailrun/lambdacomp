{-# LANGUAGE PatternSynonyms #-}
module LambdaComp.Elaborated.Syntax
  ( module LambdaComp.Const
  , module LambdaComp.Elaborated.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Data.String (IsString (..))

import LambdaComp.Const
import LambdaComp.Ident
import LambdaComp.PrimOp (PrimOp (..), PrimOpArity (..))

type Program = [Top]

data Top where
  TopTmDef :: { tmDefName :: Ident, tmDefBody :: Tm } -> Top
  deriving stock (Eq, Ord, Show)

data Tp where
  TpConst  :: !TpConst -> Tp
  (:->:)   :: Tp -> Tp -> Tp
  deriving stock (Eq, Ord, Show)
infixr 8 :->:

pattern TpFun :: Tp -> Tp -> Tp
pattern TpFun tpP tpR = tpP :->: tpR
{-# COMPLETE TpConst, TpFun #-}

data Param where
  Param :: { paramName :: !Ident, paramType :: !Tp } -> Param
  deriving stock (Eq, Ord, Show)

data Tm where
  TmVar         :: !Ident -> Tm
  TmGlobal      :: !Ident -> Tm
  TmConst       :: !TmConst -> Tm
  TmIf          :: Tm -> Tm -> Tm -> Tm
  TmLam         :: !Param -> Tm -> Tm
  TmApp         :: Tm -> Tm -> Tm
  TmPrimBinOp   :: !(PrimOp Binary) -> Tm -> Tm -> Tm
  TmPrimUnOp    :: !(PrimOp Unary) -> Tm -> Tm
  TmPrintInt    :: Tm -> Tm -> Tm
  TmPrintDouble :: Tm -> Tm -> Tm
  TmRec         :: !Param -> Tm -> Tm
  deriving stock (Eq, Ord, Show)

instance IsString Tm where
  fromString = TmVar . fromString
