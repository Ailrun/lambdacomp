{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.Optimization.BindingConversion
  ( runCommutingTo
  , runLiftingLet
  ) where

import Control.Monad.Writer.Strict (MonadWriter (tell), Writer, runWriter)
import Data.Set                    (Set)
import Data.Set                    qualified as Set

import LambdaComp.CBPV.Syntax

runCommutingTo :: Tm Com -> Tm Com
runCommutingTo = closedCommutingTo

runLiftingLet :: Tm Com -> Tm Com
runLiftingLet = closedLiftingLet

data TmToPrefix = TmToPrefix (Set Ident) (Tm Com) Ident

type family CommutingTo (c :: Class) a where
  CommutingTo Com a = Writer [TmToPrefix] a
  CommutingTo Val a = a

closedCommutingTo :: Tm Com -> Tm Com
closedCommutingTo = uncurry commitTo . runWriter . commutingTo

commutingToUnder :: Ident -> Tm Com -> CommutingTo Com (Tm Com)
commutingToUnder x = uncurry (commitToUnder x) . runWriter . commutingTo

commutingTo :: Tm c -> CommutingTo c (Tm c)
commutingTo tm@(TmVar _)             = tm
commutingTo tm@(TmGlobal _)          = tm
commutingTo tm@(TmConst _)           = tm
commutingTo (TmThunk tm)             = TmThunk $ closedCommutingTo tm
commutingTo (TmIf tm0 tm1 tm2)       = pure $ TmIf (commutingTo tm0) (closedCommutingTo tm1) (closedCommutingTo tm2)
commutingTo (TmLam x tm)             = TmLam x <$> commutingToUnder (paramName x) tm
commutingTo (tmf `TmApp` tma)        = (`TmApp` commutingTo tma) <$> commutingTo tmf
commutingTo (TmForce tm)             = pure . TmForce $ commutingTo tm
commutingTo (TmReturn tm)            = pure . TmReturn $ commutingTo tm
commutingTo (TmTo tm0 x tm1)         = do
  tm0' <- commutingTo tm0
  tell [TmToPrefix (freeVarOfTm tm0') tm0' x]
  commutingTo tm1
commutingTo (TmLet x tm0 tm1)        = TmLet x (commutingTo tm0) <$> commutingToUnder x tm1
commutingTo (TmPrimBinOp op tm0 tm1) = pure $ TmPrimBinOp op (commutingTo tm0) (commutingTo tm1)
commutingTo (TmPrimUnOp op tm)       = pure $ TmPrimUnOp op (commutingTo tm)
commutingTo (TmPrintInt tm0 tm1)     = TmPrintInt (commutingTo tm0) <$> commutingTo tm1
commutingTo (TmPrintDouble tm0 tm1)  = TmPrintDouble (commutingTo tm0) <$> commutingTo tm1
commutingTo (TmRec p tm)             = TmRec p <$> commutingToUnder (paramName p) tm

commitToUnder :: Ident -> Tm Com -> [TmToPrefix] -> CommutingTo Com (Tm Com)
commitToUnder x tm1 prefixes = do
  tell prefixes0
  pure $ foldr (\(TmToPrefix _ tm0 y) -> TmTo tm0 y) tm1 prefixes1
  where
    (prefixes0, prefixes1) = break (\(TmToPrefix fv _ y) -> x `Set.member` fv || y == x) prefixes

commitTo :: Tm Com -> [TmToPrefix] -> Tm Com
commitTo = foldr (\(TmToPrefix _ tm0 x) -> TmTo tm0 x)

data TmLetPrefix = TmLetPrefix (Set Ident) Ident (Tm Val)

type family LiftingLet (c :: Class) a where
  LiftingLet Com a = Writer [TmLetPrefix] a
  LiftingLet Val a = a

closedLiftingLet :: Tm Com -> Tm Com
closedLiftingLet = uncurry commitLet . runWriter . liftingLet

liftingLetUnder :: Ident -> Tm Com -> LiftingLet Com (Tm Com)
liftingLetUnder x = uncurry (commitLetUnder x) . runWriter . liftingLet

liftingLet :: Tm c -> LiftingLet c (Tm c)
liftingLet tm@(TmVar _)             = tm
liftingLet tm@(TmGlobal _)          = tm
liftingLet tm@(TmConst _)           = tm
liftingLet (TmThunk tm)             = TmThunk $ closedLiftingLet tm
liftingLet (TmIf tm0 tm1 tm2)       = liftA2 (TmIf $ liftingLet tm0) (liftingLet tm1) (liftingLet tm2)
liftingLet (TmLam x tm)             = TmLam x <$> liftingLetUnder (paramName x) tm
liftingLet (tmf `TmApp` tma)        = (`TmApp` liftingLet tma) <$> liftingLet tmf
liftingLet (TmForce tm)             = pure . TmForce $ liftingLet tm
liftingLet (TmReturn tm)            = pure . TmReturn $ liftingLet tm
liftingLet (TmTo tm0 x tm1)         = do
  tm0' <- liftingLet tm0
  TmTo tm0' x <$> liftingLetUnder x tm1
liftingLet (TmLet x tm0 tm1)        = do
  tell [TmLetPrefix (freeVarOfTm tm0) x (liftingLet tm0)]
  liftingLet tm1
liftingLet (TmPrimBinOp op tm0 tm1) = pure $ TmPrimBinOp op (liftingLet tm0) (liftingLet tm1)
liftingLet (TmPrimUnOp op tm)       = pure $ TmPrimUnOp op (liftingLet tm)
liftingLet (TmPrintInt tm0 tm1)     = TmPrintInt (liftingLet tm0) <$> liftingLet tm1
liftingLet (TmPrintDouble tm0 tm1)  = TmPrintDouble (liftingLet tm0) <$> liftingLet tm1
liftingLet (TmRec p tm)             = TmRec p <$> liftingLetUnder (paramName p) tm

commitLetUnder :: Ident -> Tm Com -> [TmLetPrefix] -> LiftingLet Com (Tm Com)
commitLetUnder x tm1 prefixes = do
  tell prefixes0
  pure $ foldr (\(TmLetPrefix _ y tm0) -> TmLet y tm0) tm1 prefixes1
  where
    (prefixes0, prefixes1) = break (\(TmLetPrefix fv y _) -> x `Set.member` fv || y == x) prefixes

commitLet :: Tm Com -> [TmLetPrefix] -> Tm Com
commitLet = foldr (\(TmLetPrefix _ tm0 x) -> TmLet tm0 x)
