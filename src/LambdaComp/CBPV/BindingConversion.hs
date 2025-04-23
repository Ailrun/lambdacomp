{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.BindingConversion where

import Control.Monad.Writer.Strict (MonadWriter (tell), Writer, runWriter)
import Data.Kind                   (Type)
import Data.Set                    (Set)
import Data.Set                    qualified as Set

import LambdaComp.CBPV.Syntax

topCommutingThen :: Tm Com -> Tm Com
topCommutingThen = uncurry commitThen . runWriter . commutingThen

topLiftingLet :: Tm Com -> Tm Com
topLiftingLet = uncurry commitLet . runWriter . liftingLet

data TmThenPrefix = TmThenPrefix (Set Ident) (Tm Com) Ident

type family CommutingThen (c :: Class) (a :: Type) where
  CommutingThen Com a = Writer [TmThenPrefix] a
  CommutingThen Val a = a

commutingThenUnder :: Ident -> Tm Com -> CommutingThen Com (Tm Com)
commutingThenUnder x = uncurry (commitThenUnder x) . runWriter . commutingThen

commutingThen :: Tm c -> CommutingThen c (Tm c)
commutingThen tm@(TmVar _)         = tm
commutingThen tm@TmUnit            = tm
commutingThen tm@(TmInt _)         = tm
commutingThen tm@(TmDouble _)      = tm
commutingThen (TmThunk tm)         = TmThunk $ topCommutingThen tm
commutingThen (TmForce tm)         = pure . TmForce $ commutingThen tm
commutingThen (TmLam x tm)         = TmLam x <$> commutingThenUnder x tm
commutingThen (tmf `TmApp` tma)    = (`TmApp` commutingThen tma) <$> commutingThen tmf
commutingThen (TmReturn tm)        = pure . TmReturn $ commutingThen tm
commutingThen (TmThen tm0 x tm1)   = do
  tm0' <- commutingThen tm0
  tell [TmThenPrefix (freeVarOfTm tm0') tm0' x]
  commutingThen tm1
commutingThen (TmLet x tm0 tm1)    = TmLet x (commutingThen tm0) <$> commutingThenUnder x tm1
commutingThen (TmPrintInt tm0 tm1) = TmPrintInt (commutingThen tm0) <$> commutingThen tm1
commutingThen (TmRec f tm)         = TmRec f <$> commutingThenUnder f tm

commitThenUnder :: Ident -> Tm Com -> [TmThenPrefix] -> CommutingThen Com (Tm Com)
commitThenUnder x tm1 prefixes = do
  tell prefixes0
  pure $ foldr (\(TmThenPrefix _ tm0 y) -> TmThen tm0 y) tm1 prefixes1
  where
    (prefixes0, prefixes1) = break (\(TmThenPrefix fv _ y) -> x `Set.member` fv || y == x) prefixes

commitThen :: Tm Com -> [TmThenPrefix] -> Tm Com
commitThen = foldr (\(TmThenPrefix _ tm0 x) -> TmThen tm0 x)

data TmLetPrefix = TmLetPrefix (Set Ident) Ident (Tm Val)

type family LiftingLet (c :: Class) (a :: Type) where
  LiftingLet Com a = Writer [TmLetPrefix] a
  LiftingLet Val a = a

liftingLetUnder :: Ident -> Tm Com -> LiftingLet Com (Tm Com)
liftingLetUnder x = uncurry (commitLetUnder x) . runWriter . liftingLet

liftingLet :: Tm c -> LiftingLet c (Tm c)
liftingLet tm@(TmVar _)         = tm
liftingLet tm@TmUnit            = tm
liftingLet tm@(TmInt _)         = tm
liftingLet tm@(TmDouble _)      = tm
liftingLet (TmThunk tm)         = TmThunk $ topLiftingLet tm
liftingLet (TmForce tm)         = pure . TmForce $ liftingLet tm
liftingLet (TmLam x tm)         = TmLam x <$> liftingLetUnder x tm
liftingLet (tmf `TmApp` tma)    = (`TmApp` liftingLet tma) <$> liftingLet tmf
liftingLet (TmReturn tm)        = pure . TmReturn $ liftingLet tm
liftingLet (TmThen tm0 x tm1)   = do
  tm0' <- liftingLet tm0
  TmThen tm0' x <$> liftingLetUnder x tm1
liftingLet (TmLet x tm0 tm1)    = do
  tell [TmLetPrefix (freeVarOfTm tm0) x (liftingLet tm0)]
  liftingLet tm1
liftingLet (TmPrintInt tm0 tm1) = TmPrintInt (liftingLet tm0) <$> liftingLet tm1
liftingLet (TmRec f tm)         = TmRec f <$> liftingLetUnder f tm

commitLetUnder :: Ident -> Tm Com -> [TmLetPrefix] -> LiftingLet Com (Tm Com)
commitLetUnder x tm1 prefixes = do
  tell prefixes0
  pure $ foldr (\(TmLetPrefix _ y tm0) -> TmLet y tm0) tm1 prefixes1
  where
    (prefixes0, prefixes1) = break (\(TmLetPrefix fv y _) -> x `Set.member` fv || y == x) prefixes

commitLet :: Tm Com -> [TmLetPrefix] -> Tm Com
commitLet = foldr (\(TmLetPrefix _ tm0 x) -> TmLet tm0 x)
