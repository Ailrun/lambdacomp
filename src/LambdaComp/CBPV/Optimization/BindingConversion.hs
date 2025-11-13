{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.Optimization.BindingConversion
  ( runCommutingTo
  , runLiftingLet
  ) where

import Control.Monad.Writer.Strict (MonadWriter (tell), Writer, lift, runWriter)
import Data.Functor.Identity       (Identity (Identity))
import Data.Set                    (Set)
import Data.Set                    qualified as Set

import LambdaComp.Binder      (getBoundVar, lensBinderBody)
import LambdaComp.CBPV.Syntax

runCommutingTo :: Tm Com -> Tm Com
runCommutingTo = closedCommutingTo

runLiftingLet :: Tm Com -> Tm Com
runLiftingLet = closedLiftingLet

------------------------------------------------------------
-- lifting/commuting "to"s

closedCommutingTo :: Tm Com -> Tm Com
closedCommutingTo = uncurry commitTo . runWriter . commutingTo

commutingToUnder :: Binder t -> CommutingTo Com (Binder t)
commutingToUnder b = lensBinderBody (uncurry (commitToUnder $ getBoundVar b) . runWriter . commutingTo) b

data TmToPrefix = TmToPrefix (Set Ident) (Tm Com) Ident

class CommutingToClass (c :: Class) where
  type CommutingTo (c :: Class) a
  commutingTo :: Tm c -> CommutingTo c (Tm c)

instance CommutingToClass Com where
  type CommutingTo Com a = Writer [TmToPrefix] a

  commutingTo tm@(TmLam _; TmApp {}
                 ; TmForce _
                 ; TmReturn _
                 ; TmLet {}
                 ; TmPrimBinOp {}; TmPrimUnOp {}
                 ; TmRec _)           = recTmBM (Identity . commutingTo) commutingTo commutingToUnder lift tm
  commutingTo (TmIf tm0 tm1 tm2)      = pure $ TmIf (commutingTo tm0) (closedCommutingTo tm1) (closedCommutingTo tm2)
  commutingTo (TmTo tm0 b)            = do
    tm0' <- commutingTo tm0
    tell [TmToPrefix (freeVarOfTm tm0') tm0' x]
    commutingTo tm1
    where
      BUntyped x tm1 = b
  commutingTo (TmPrintInt tm0 tm1)    = pure $ TmPrintInt (commutingTo tm0) (closedCommutingTo tm1)
  commutingTo (TmPrintDouble tm0 tm1) = pure $ TmPrintDouble (commutingTo tm0) (closedCommutingTo tm1)

instance CommutingToClass Val where
  type CommutingTo Val a = a

  commutingTo = recTm commutingTo closedCommutingTo

commitToUnder :: Ident -> Tm Com -> [TmToPrefix] -> CommutingTo Com (Tm Com)
commitToUnder x tm1 prefixes = do
  tell prefixes0
  pure $ commitTo tm1 prefixes1
  where
    (prefixes0, prefixes1) = break (\(TmToPrefix fv _ y) -> x `Set.member` fv || y == x) prefixes

commitTo :: Tm Com -> [TmToPrefix] -> Tm Com
commitTo = foldr (\(TmToPrefix _ tm0 x) -> TmTo tm0 . BUntyped x)

------------------------------------------------------------
-- lifting/commuting "let"s

closedLiftingLet :: Tm Com -> Tm Com
closedLiftingLet = uncurry commitLet . runWriter . liftingLet

liftingLetUnder :: Binder t -> LiftingLet Com (Binder t)
liftingLetUnder b = lensBinderBody (uncurry (commitLetUnder $ getBoundVar b) . runWriter . liftingLet) b

data TmLetPrefix = TmLetPrefix (Set Ident) (Tm Val) Ident

class LiftingLetClass (c :: Class) where
  type LiftingLet (c :: Class) a

  liftingLet :: Tm c -> LiftingLet c (Tm c)

instance LiftingLetClass Com where
  type LiftingLet Com a = Writer [TmLetPrefix] a

  liftingLet tm@(TmIf {}
                ; TmLam _; TmApp {}
                ; TmForce _
                ; TmReturn _; TmTo {}
                ; TmPrimBinOp {}; TmPrimUnOp {}
                ; TmPrintInt {}; TmPrintDouble {}
                ; TmRec _) = recTmBM (Identity . liftingLet) liftingLet liftingLetUnder lift tm
  liftingLet (TmLet tm0 b) = do
    tell [TmLetPrefix (freeVarOfTm tm0) (liftingLet tm0) x]
    liftingLet tm1
    where
      BUntyped x tm1 = b

instance LiftingLetClass Val where
  type LiftingLet Val a = a

  liftingLet = recTm liftingLet closedLiftingLet

commitLetUnder :: Ident -> Tm Com -> [TmLetPrefix] -> LiftingLet Com (Tm Com)
commitLetUnder x tm1 prefixes = do
  tell prefixes0
  pure $ commitLet tm1 prefixes1
  where
    (prefixes0, prefixes1) = break (\(TmLetPrefix fv _ y) -> x `Set.member` fv || y == x) prefixes

commitLet :: Tm Com -> [TmLetPrefix] -> Tm Com
commitLet = foldr (\(TmLetPrefix _ tm0 x) -> TmLet tm0 . BUntyped x)
