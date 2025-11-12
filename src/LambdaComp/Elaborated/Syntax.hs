{-# LANGUAGE PatternSynonyms #-}
module LambdaComp.Elaborated.Syntax
  ( module LambdaComp.Const
  , module LambdaComp.Elaborated.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Control.Applicative (liftA3)
import Data.String         (IsString (..))

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

recTmM :: (Applicative f) => (Tm -> f Tm) -> Tm -> f Tm
recTmM _ tm@(TmVar _; TmGlobal _; TmConst _) = pure tm
recTmM f (TmIf tm0 tm1 tm2)                  = liftA3 TmIf (f tm0) (f tm1) (f tm2)
recTmM f (TmLam p tm)                        = TmLam p <$> f tm
recTmM f (tmf `TmApp` tma)                   = liftA2 TmApp (f tmf) (f tma)
recTmM f (TmPrimBinOp bop tm0 tm1)           = liftA2 (TmPrimBinOp bop) (f tm0) (f tm1)
recTmM f (TmPrimUnOp uop tm)                 = TmPrimUnOp uop <$> f tm
recTmM f (TmPrintInt tm0 tm1)                = liftA2 TmPrintInt (f tm0) (f tm1)
recTmM f (TmPrintDouble tm0 tm1)             = liftA2 TmPrintDouble (f tm0) (f tm1)
recTmM f (TmRec p tm)                        = TmRec p <$> f tm
