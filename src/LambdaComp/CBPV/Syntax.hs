{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeData        #-}
{-# LANGUAGE TypeFamilies    #-}
module LambdaComp.CBPV.Syntax
  ( module LambdaComp.Binder
  , module LambdaComp.CBPV.Syntax
  , module LambdaComp.Const
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Control.Applicative   (Const (Const, getConst), liftA3)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Set              (Set)
import Data.Set              qualified as Set

import LambdaComp.Binder (BTTyped, BTUntyped, paramName, paramType, pattern BTyped, pattern BUntyped, pattern Param)
import LambdaComp.Binder qualified as B
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

type Param = B.Param (Tp Val)
type Binder t = B.Binder t (Tp Val) (Tm Com)

type role Tm nominal
data Tm (c :: Class) where
  TmVar    :: !Ident -> Tm Val
  TmGlobal :: !Ident -> Tm Val
  TmConst  :: !TmConst -> Tm Val

  TmThunk  :: Tm Com -> Tm Val

  TmIf          :: Tm Val -> Tm Com -> Tm Com -> Tm Com

  TmLam         :: !(Binder BTTyped) -> Tm Com
  TmApp         :: Tm Com -> Tm Val -> Tm Com

  TmForce       :: Tm Val -> Tm Com

  TmReturn      :: Tm Val -> Tm Com
  TmTo          :: Tm Com -> !(Binder BTUntyped) -> Tm Com

  TmLet         :: Tm Val -> !(Binder BTUntyped) -> Tm Com

  TmPrimBinOp   :: !(PrimOp Binary) -> Tm Val -> Tm Val -> Tm Com
  TmPrimUnOp    :: !(PrimOp Unary) -> Tm Val -> Tm Com

  TmPrintInt    :: Tm Val -> Tm Com -> Tm Com
  TmPrintDouble :: Tm Val -> Tm Com -> Tm Com

  TmRec         :: !(Binder BTTyped) -> Tm Com

deriving stock instance Eq (Tm c)
deriving stock instance Ord (Tm c)
deriving stock instance Show (Tm c)

recTmBM :: (Applicative g) => (Tm Val -> f (Tm Val)) -> (Tm Com -> g (Tm Com)) -> (forall t. Binder t -> g (Binder t)) -> (f (Tm Val) -> g (Tm Val)) -> Tm c -> g (Tm c)
recTmBM _ _ _ _ tm@(TmVar _; TmGlobal _; TmConst _) = pure tm
recTmBM _ g _ _ (TmThunk tm)                        = TmThunk <$> g tm
recTmBM f g _ t (TmIf tm0 tm1 tm2)                  = liftA3 TmIf (t $ f tm0) (g tm1) (g tm2)
recTmBM _ _ h _ (TmLam b)                           = TmLam <$> h b
recTmBM f g _ t (tmf `TmApp` tma)                   = liftA2 TmApp (g tmf) (t $ f tma)
recTmBM f _ _ t (TmForce tm)                        = TmForce <$> t (f tm)
recTmBM f _ _ t (TmReturn tm)                       = TmReturn <$> t (f tm)
recTmBM _ g h _ (TmTo tm0 b)                        = liftA2 TmTo (g tm0) (h b)
recTmBM f _ h t (TmLet tm0 b)                       = liftA2 TmLet (t $ f tm0) (h b)
recTmBM f _ _ t (TmPrimBinOp bop tm0 tm1)           = liftA2 (TmPrimBinOp bop) (t $ f tm0) (t $ f tm1)
recTmBM f _ _ t (TmPrimUnOp uop tm)                 = TmPrimUnOp uop <$> t (f tm)
recTmBM f g _ t (TmPrintInt tm0 tm1)                = liftA2 TmPrintInt (t $ f tm0) (g tm1)
recTmBM f g _ t (TmPrintDouble tm0 tm1)             = liftA2 TmPrintDouble (t $ f tm0) (g tm1)
recTmBM _ _ h _ (TmRec b)                           = TmRec <$> h b

recTmM :: forall f g c. (Applicative g) => (Tm Val -> f (Tm Val)) -> (Tm Com -> g (Tm Com)) -> (f (Tm Val) -> g (Tm Val)) -> Tm c -> g (Tm c)
recTmM ff gf = recTmBM ff gf helper
  where
    helper :: Binder t -> g (Binder t)
    helper (BUntyped p tm) = BUntyped p <$> gf tm
    helper (BTyped x tm)   = BTyped x <$> gf tm

recTmB :: (Tm Val -> Tm Val) -> (Tm Com -> Tm Com) -> (forall t. Binder t -> Binder t) -> Tm c -> Tm c
recTmB f g h = runIdentity . recTmBM (Identity . f) (Identity . g) (Identity . h) id

recTm :: (Tm Val -> Tm Val) -> (Tm Com -> Tm Com) -> Tm c -> Tm c
recTm f g = runIdentity . recTmM (Identity . f) (Identity . g) id

polyRecTmBM :: (Applicative f) => (forall c. Tm c -> f (Tm c)) -> (forall t. Binder t -> f (Binder t)) -> Tm c' -> f (Tm c')
polyRecTmBM f h = recTmBM f f h id

polyRecTmM :: (Applicative f) => (forall c. Tm c -> f (Tm c)) -> Tm c' -> f (Tm c')
polyRecTmM f = recTmM f f id

polyRecTmB :: (forall c. Tm c -> Tm c) -> (forall t. Binder t -> Binder t) -> Tm c' -> Tm c'
polyRecTmB f = recTmB f f

polyRecTm :: (forall c. Tm c -> Tm c) -> Tm c' -> Tm c'
polyRecTm f = recTm f f

freeVarOfTm :: Tm c -> Set Ident

freeVarOfTm tm@(TmGlobal _
               ; TmConst _
               ; TmThunk _) = getConst $ polyRecTmM (Const . freeVarOfTm) tm
freeVarOfTm (TmVar x)       = Set.singleton x

freeVarOfTm tm@(TmIf {}
               ; TmLam {}; TmApp {}
               ; TmForce _
               ; TmReturn _; TmTo {}
               ; TmLet {}
               ; TmPrimBinOp {}; TmPrimUnOp {}
               ; TmPrintInt {}; TmPrintDouble {}
               ; TmRec {}) = getConst $ polyRecTmBM (Const . freeVarOfTm) (Const . freeVarOfBinder) tm

freeVarOfBinder :: Binder t -> Set Ident
freeVarOfBinder b = B.getBoundVar b `Set.delete` freeVarOfTm (B.getBinderBody b)

freeVarAndGlobalOfTm :: Tm c -> Set Ident

freeVarAndGlobalOfTm tm@(TmConst _
                        ; TmThunk _) = getConst $ polyRecTmM (Const . freeVarAndGlobalOfTm) tm
freeVarAndGlobalOfTm (TmVar x)       = Set.singleton x
freeVarAndGlobalOfTm (TmGlobal x)    = Set.singleton x

freeVarAndGlobalOfTm tm@(TmIf {}
                        ; TmLam {}; TmApp {}
                        ; TmForce _
                        ; TmReturn _; TmTo {}
                        ; TmLet {}
                        ; TmPrimBinOp {}; TmPrimUnOp {}
                        ; TmPrintInt {}; TmPrintDouble {}
                        ; TmRec {}) = getConst $ polyRecTmBM (Const . freeVarAndGlobalOfTm) (Const . freeVarAndGlobalOfBinder) tm

freeVarAndGlobalOfBinder :: Binder t -> Set Ident
freeVarAndGlobalOfBinder b = B.getBoundVar b `Set.delete` freeVarAndGlobalOfTm (B.getBinderBody b)
