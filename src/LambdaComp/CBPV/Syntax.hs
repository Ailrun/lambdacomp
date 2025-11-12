{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeData        #-}
module LambdaComp.CBPV.Syntax
  ( module LambdaComp.CBPV.Syntax
  , module LambdaComp.Const
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Control.Applicative (Const (Const, getConst), liftA3)
import Data.Set            (Set)
import Data.Set            qualified as Set

import LambdaComp.Const
import LambdaComp.Ident
import LambdaComp.PrimOp (PrimOp (..), PrimOpArity (..))

type data Class where
  Val, Com :: Class

type Program = [Top]

data Top where
  TopTmDef :: { tmDefName :: Ident, tmDefBody :: Tm Com } -> Top
  deriving stock (Eq, Ord, Show)

type role Tp nominal
data Tp (c :: Class) where
  TpConst  :: !TpConst -> Tp Val
  TpUp     :: Tp Com -> Tp Val

  (:->:) :: Tp Val -> Tp Com -> Tp Com
  TpDown :: Tp Val -> Tp Com
deriving stock instance Eq (Tp c)
deriving stock instance Ord (Tp c)
deriving stock instance Show (Tp c)
infixr 8 :->:
pattern TpFun :: Tp Val -> Tp Com -> Tp Com
pattern TpFun tp0 tp1 = tp0 :->: tp1
{-# COMPLETE TpConst, TpUp, TpFun, TpDown #-}

data Param where
  Param :: { paramName :: !Ident, paramType :: !(Tp Val) } -> Param
  deriving stock (Eq, Ord, Show)

type role Tm nominal
data Tm (c :: Class) where
  TmVar    :: !Ident -> Tm Val
  TmGlobal :: !Ident -> Tm Val
  TmConst  :: !TmConst -> Tm Val

  TmThunk  :: Tm Com -> Tm Val

  TmIf          :: Tm Val -> Tm Com -> Tm Com -> Tm Com

  TmLam         :: !Param -> Tm Com -> Tm Com
  TmApp         :: Tm Com -> Tm Val -> Tm Com

  TmForce       :: Tm Val -> Tm Com

  TmReturn      :: Tm Val -> Tm Com
  TmTo          :: Tm Com -> Ident -> Tm Com -> Tm Com

  TmLet         :: !Ident -> Tm Val -> Tm Com -> Tm Com

  TmPrimBinOp   :: !(PrimOp Binary) -> Tm Val -> Tm Val -> Tm Com
  TmPrimUnOp    :: !(PrimOp Unary) -> Tm Val -> Tm Com

  TmPrintInt    :: Tm Val -> Tm Com -> Tm Com
  TmPrintDouble :: Tm Val -> Tm Com -> Tm Com

  TmRec         :: !Param -> Tm Com -> Tm Com

deriving stock instance Eq (Tm c)
deriving stock instance Ord (Tm c)
deriving stock instance Show (Tm c)

polyRecTmM :: (Applicative f) => (forall c. Tm c -> f (Tm c)) -> Tm c' -> f (Tm c')
polyRecTmM _ tm@(TmVar _; TmGlobal _; TmConst _) = pure tm
polyRecTmM f (TmThunk tm)                        = TmThunk <$> f tm
polyRecTmM f (TmIf tm0 tm1 tm2)                  = liftA3 TmIf (f tm0) (f tm1) (f tm2)
polyRecTmM f (TmLam p tm)                        = TmLam p <$> f tm
polyRecTmM f (tmf `TmApp` tma)                   = liftA2 TmApp (f tmf) (f tma)
polyRecTmM f (TmForce tm)                        = TmForce <$> f tm
polyRecTmM f (TmReturn tm)                       = TmReturn <$> f tm
polyRecTmM f (TmTo tm0 x tm1)                    = liftA2 (`TmTo` x) (f tm0) (f tm1)
polyRecTmM f (TmLet x tm0 tm1)                   = liftA2 (TmLet x) (f tm0) (f tm1)
polyRecTmM f (TmPrimBinOp bop tm0 tm1)           = liftA2 (TmPrimBinOp bop) (f tm0) (f tm1)
polyRecTmM f (TmPrimUnOp uop tm)                 = TmPrimUnOp uop <$> f tm
polyRecTmM f (TmPrintInt tm0 tm1)                = liftA2 TmPrintInt (f tm0) (f tm1)
polyRecTmM f (TmPrintDouble tm0 tm1)             = liftA2 TmPrintDouble (f tm0) (f tm1)
polyRecTmM f (TmRec p tm)                        = TmRec p <$> f tm

freeVarOfTm :: Tm c -> Set Ident

freeVarOfTm tm@(TmGlobal _
               ; TmConst _
               ; TmThunk _) = getConst $ polyRecTmM (Const . freeVarOfTm) tm
freeVarOfTm (TmVar x)       = Set.singleton x

freeVarOfTm tm@(TmIf {}
               ; TmApp {}
               ; TmForce _
               ; TmReturn _
               ; TmPrimBinOp {}; TmPrimUnOp {}
               ; TmPrintInt {}; TmPrintDouble {}) = getConst $ polyRecTmM (Const . freeVarOfTm) tm
freeVarOfTm (TmLam p tm)                          = paramName p `Set.delete` freeVarOfTm tm
freeVarOfTm (TmTo tm0 x tm1)                      = freeVarOfTm tm0 `Set.union` (x `Set.delete` freeVarOfTm tm1)
freeVarOfTm (TmLet x tm0 tm1)                     = freeVarOfTm tm0 `Set.union` (x `Set.delete` freeVarOfTm tm1)
freeVarOfTm (TmRec p tm)                          = paramName p `Set.delete` freeVarOfTm tm

freeVarAndGlobalOfTm :: Tm c -> Set Ident

freeVarAndGlobalOfTm tm@(TmConst _
                        ; TmThunk _) = getConst $ polyRecTmM (Const . freeVarAndGlobalOfTm) tm
freeVarAndGlobalOfTm (TmVar x)       = Set.singleton x
freeVarAndGlobalOfTm (TmGlobal x)    = Set.singleton x

freeVarAndGlobalOfTm tm@(TmIf {}
                        ; TmApp {}
                        ; TmForce _
                        ; TmReturn _
                        ; TmPrimBinOp {}; TmPrimUnOp {}
                        ; TmPrintInt {}; TmPrintDouble {}) = getConst $ polyRecTmM (Const . freeVarAndGlobalOfTm) tm
freeVarAndGlobalOfTm (TmLam p tm)                          = paramName p `Set.delete` freeVarAndGlobalOfTm tm
freeVarAndGlobalOfTm (TmTo tm0 x tm1)                      = freeVarAndGlobalOfTm tm0 `Set.union` (x `Set.delete` freeVarAndGlobalOfTm tm1)
freeVarAndGlobalOfTm (TmLet x tm0 tm1)                     = freeVarAndGlobalOfTm tm0 `Set.union` (x `Set.delete` freeVarAndGlobalOfTm tm1)
freeVarAndGlobalOfTm (TmRec p tm)                          = paramName p `Set.delete` freeVarAndGlobalOfTm tm
