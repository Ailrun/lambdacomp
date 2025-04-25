{-# LANGUAGE GADTs    #-}
{-# LANGUAGE TypeData #-}
module LambdaComp.CBPV.Syntax
  ( module LambdaComp.CBPV.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

import LambdaComp.Ident
import LambdaComp.PrimOp (PrimOp(..), PrimOpArity (..))

type data Class where
  Val, Com :: Class

type Program = [Top]

data Top
  = TopTmDef
    { tmDefName :: Ident
    , tmDefType :: Tp Val
    , tmDefBody :: Tm Val
    }
  deriving stock (Show)

data Tp (c :: Class) where
  TpUnit   :: Tp Val
  TpBool   :: Tp Val
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
  TmVar    :: !Ident -> Tm Val
  TmUnit   :: Tm Val
  TmTrue   :: Tm Val
  TmFalse  :: Tm Val
  TmInt    :: !Int -> Tm Val
  TmDouble :: !Double -> Tm Val

  TmThunk  :: Tm Com -> Tm Val

  TmIf        :: Tm Val -> Tm Com -> Tm Com -> Tm Com

  TmLam       :: !Ident -> Tm Com -> Tm Com
  TmApp       :: Tm Com -> Tm Val -> Tm Com

  TmForce     :: Tm Val -> Tm Com

  TmReturn    :: Tm Val -> Tm Com
  TmTo        :: Tm Com -> Ident -> Tm Com -> Tm Com

  TmLet       :: !Ident -> Tm Val -> Tm Com -> Tm Com

  TmPrimBinOp :: !(PrimOp Binary) -> Tm Val -> Tm Val -> Tm Com
  TmPrimUnOp  :: !(PrimOp Unary) -> Tm Val -> Tm Com

  TmPrintInt  :: Tm Val -> Tm Com -> Tm Com

  TmRec       :: !Ident -> Tm Com -> Tm Com

deriving stock instance Eq (Tm c)
deriving stock instance Ord (Tm c)
deriving stock instance Show (Tm c)

instance Read (Tm c) where
  readsPrec = undefined

freeVarOfTm :: Tm c -> Set Ident
freeVarOfTm (TmVar x)               = Set.singleton x
freeVarOfTm (TmThunk tm)            = freeVarOfTm tm
freeVarOfTm (TmIf tm0 tm1 tm2)      = Set.unions [freeVarOfTm tm0, freeVarOfTm tm1, freeVarOfTm tm2]
freeVarOfTm (TmLam x tm)            = x `Set.delete` freeVarOfTm tm
freeVarOfTm (tmf `TmApp` tma)       = freeVarOfTm tmf `Set.union` freeVarOfTm tma
freeVarOfTm (TmForce tm)            = freeVarOfTm tm
freeVarOfTm (TmReturn tm)           = freeVarOfTm tm
freeVarOfTm (TmTo tm0 x tm1)        = freeVarOfTm tm0 `Set.union` (x `Set.delete` freeVarOfTm tm1)
freeVarOfTm (TmLet x tm0 tm1)       = freeVarOfTm tm0 `Set.union` (x `Set.delete` freeVarOfTm tm1)
freeVarOfTm (TmPrimBinOp _ tm0 tm1) = freeVarOfTm tm0 `Set.union` freeVarOfTm tm1
freeVarOfTm (TmPrimUnOp _ tm)       = freeVarOfTm tm
freeVarOfTm (TmPrintInt tm0 tm1)    = freeVarOfTm tm0 `Set.union` freeVarOfTm tm1
freeVarOfTm (TmRec x tm)            = x `Set.delete` freeVarOfTm tm
freeVarOfTm _                       = Set.empty -- ground values
