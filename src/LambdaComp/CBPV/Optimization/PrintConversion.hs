{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.Optimization.PrintConversion
  ( runCommutingPrint
  ) where


import Control.Monad.Writer.Strict (MonadWriter (tell), Writer, lift, runWriter)
import Data.Functor.Identity       (Identity (Identity))
import Data.Set                    qualified as Set

import LambdaComp.Binder      (getBoundVar, lensBinderBody, mapBinderBody)
import LambdaComp.CBPV.Syntax

runCommutingPrint :: Tm Com -> Tm Com
runCommutingPrint = closedCommutingPrint

------------------------------------------------------------
-- lifting/commuting printInt

closedCommutingPrint :: Tm Com -> Tm Com
closedCommutingPrint = uncurry commitPrint . runWriter . commutingPrint

commutingPrintUnder :: Binder t -> CommutingPrint Com (Binder t)
commutingPrintUnder b = lensBinderBody (uncurry (commitPrintUnder $ getBoundVar b) . runWriter . commutingPrint) b

data PrintVal where
  PrintIntVal, PrintDoubleVal :: Tm Val -> PrintVal

class CommutingPrintClass (c :: Class) where
  type CommutingPrint (c :: Class) a
  commutingPrint :: Tm c -> CommutingPrint c (Tm c)

instance CommutingPrintClass Com where
  type CommutingPrint Com a = Writer [PrintVal] a

  commutingPrint tm@(TmLam _; TmApp {}
                    ; TmForce _
                    ; TmReturn _
                    ; TmLet {}
                    ; TmPrimBinOp {}; TmPrimUnOp {}
                    ; TmRec _)           = recTmBM (Identity . commutingPrint) commutingPrint commutingPrintUnder lift tm
  commutingPrint (TmIf tm0 tm1 tm2)      = pure $ TmIf (commutingPrint tm0) (closedCommutingPrint tm1) (closedCommutingPrint tm2)
  commutingPrint (TmTo tm0 b)            = flip TmTo (mapBinderBody closedCommutingPrint b) <$> commutingPrint tm0
  commutingPrint (TmPrintInt tm0 tm1)    = do
    tell [PrintIntVal (commutingPrint tm0)]
    commutingPrint tm1
  commutingPrint (TmPrintDouble tm0 tm1) = do
    tell [PrintDoubleVal (commutingPrint tm0)]
    commutingPrint tm1

instance CommutingPrintClass Val where
  type CommutingPrint Val a = a

  commutingPrint = recTm commutingPrint closedCommutingPrint

commitPrintUnder :: Ident -> Tm Com -> [PrintVal] -> CommutingPrint Com (Tm Com)
commitPrintUnder x tm1 prefixes = do
  tell prefixes0
  pure $ commitPrint tm1 prefixes1
  where
    (prefixes0, prefixes1) = break breakCond prefixes

    breakCond (PrintIntVal tm0)    = x `Set.member` freeVarOfTm tm0
    breakCond (PrintDoubleVal tm0) = x `Set.member` freeVarOfTm tm0

commitPrint :: Tm Com -> [PrintVal] -> Tm Com
commitPrint = foldr go
  where
    go (PrintIntVal tm0)    = TmPrintInt tm0
    go (PrintDoubleVal tm0) = TmPrintDouble tm0
