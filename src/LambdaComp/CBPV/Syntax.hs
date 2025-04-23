{-# LANGUAGE GADTs    #-}
{-# LANGUAGE TypeData #-}
module LambdaComp.CBPV.Syntax
  ( module LambdaComp.CBPV.Syntax
  , module LambdaComp.Ident
  ) where

import Data.Set         (Set)
import Data.Set         qualified as Set

import LambdaComp.Ident

type data Class where
  Val, Com :: Class

data Tp (c :: Class) where
  TpUnit   :: Tp Val
  TpInt    :: Tp Val
  TpDouble :: Tp Val
  TpUp     :: Tp Com -> Tp Val

  TpFun  :: Tp Val -> Tp Com -> Tp Com
  TpDown :: Tp Val -> Tp Com
deriving stock instance Eq (Tp c)
deriving stock instance Ord (Tp c)
deriving stock instance Show (Tp c)

instance Read (Tp c) where
  readsPrec = undefined

data Tm (c :: Class) where
  TmVar      :: Ident -> Tm Val
  TmUnit     :: Tm Val
  TmInt      :: !Int -> Tm Val
  TmDouble   :: !Double -> Tm Val

  TmThunk    :: Tm Com -> Tm Val

  TmLam      :: Ident -> Tm Com -> Tm Com
  TmApp      :: Tm Com -> Tm Val -> Tm Com

  TmForce    :: Tm Val -> Tm Com

  TmReturn   :: Tm Val -> Tm Com
  TmThen     :: Tm Com -> Ident -> Tm Com -> Tm Com

  TmLet      :: Ident -> Tm Val -> Tm Com -> Tm Com

  TmPrintInt :: Tm Val -> Tm Com -> Tm Com

  TmRec      :: Ident -> Tm Com -> Tm Com

deriving stock instance Eq (Tm c)
deriving stock instance Ord (Tm c)
deriving stock instance Show (Tm c)

instance Read (Tm c) where
  readsPrec = undefined

freeVarOfTm :: Tm c -> Set Ident
freeVarOfTm (TmVar x)            = Set.singleton x
freeVarOfTm (TmThunk tm)         = freeVarOfTm tm
freeVarOfTm (TmLam x tm)         = x `Set.delete` freeVarOfTm tm
freeVarOfTm (tmf `TmApp` tma)    = freeVarOfTm tmf `Set.union` freeVarOfTm tma
freeVarOfTm (TmForce tm)         = freeVarOfTm tm
freeVarOfTm (TmReturn tm)        = freeVarOfTm tm
freeVarOfTm (TmThen tm0 x tm1)   = freeVarOfTm tm0 `Set.union` (x `Set.delete` freeVarOfTm tm1)
freeVarOfTm (TmLet x tm0 tm1)    = freeVarOfTm tm0 `Set.union` (x `Set.delete` freeVarOfTm tm1)
freeVarOfTm (TmPrintInt tm0 tm1) = freeVarOfTm tm0 `Set.union` freeVarOfTm tm1
freeVarOfTm (TmRec x tm)         = x `Set.delete` freeVarOfTm tm
freeVarOfTm _                    = Set.empty -- ground values
