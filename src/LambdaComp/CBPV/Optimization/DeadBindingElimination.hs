module LambdaComp.CBPV.Optimization.DeadBindingElimination
  ( runDeadLetElimination
  ) where

import Control.Monad.Writer.CPS (MonadWriter (tell), Writer, censor, listens, runWriter)
import Data.Set                 (Set)
import Data.Set                 qualified as Set

import LambdaComp.Binder      (getBoundVar, lensBinderBody)
import LambdaComp.CBPV.Syntax

runDeadLetElimination :: Tm Com -> Tm Com
runDeadLetElimination = fst . runWriter . deadLetElimination

type WithFreeVars = Writer (Set Ident)

deadLetElimination :: Tm c -> WithFreeVars (Tm c)

deadLetElimination tm@(TmGlobal _
                      ; TmConst _
                      ; TmThunk _) = polyRecTmM deadLetElimination tm
deadLetElimination tm@(TmVar x)    = tm <$ tell (Set.singleton x)

deadLetElimination tm@(TmIf {}
                      ; TmLam _; TmApp {}
                      ; TmForce _
                      ; TmReturn _; TmTo {}
                      ; TmPrimBinOp {}; TmPrimUnOp {}
                      ; TmPrintInt {}; TmPrintDouble {}
                      ; TmRec _)                = polyRecTmBM deadLetElimination deadLetEliminationBinder tm
deadLetElimination (TmLet tm0 (BUntyped x tm1)) = do
  (tm1', withX) <- without x $ listens (x `Set.member`) $ deadLetElimination tm1
  if withX
    then flip TmLet (BUntyped x tm1') <$> deadLetElimination tm0
    else pure tm1'

deadLetEliminationBinder :: Binder t -> WithFreeVars (Binder t)
deadLetEliminationBinder b = lensBinderBody (without (getBoundVar b) . deadLetElimination) b

without :: Ident -> WithFreeVars a -> WithFreeVars a
without x = censor (Set.delete x)
