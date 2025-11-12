module LambdaComp.CBPV.Optimization.DeadBindingElimination
  ( runDeadLetElimination
  ) where

import Control.Monad.Writer.CPS (MonadWriter (tell), Writer, censor, listens, runWriter)
import Data.Set                 (Set)
import Data.Set                 qualified as Set

import LambdaComp.CBPV.Syntax

runDeadLetElimination :: Tm Com -> Tm Com
runDeadLetElimination = fst . runWriter . deadLetElimination

type WithFreeVars = Writer (Set Ident)

deadLetElimination :: Tm c -> WithFreeVars (Tm c)
deadLetElimination tm@(TmGlobal _
                      ; TmConst _
                      ; TmThunk _)          = polyRecTmM deadLetElimination tm
deadLetElimination tm@(TmVar x)             = tm <$ tell (Set.singleton x)

deadLetElimination tm@(TmIf {}
                      ; TmApp {}
                      ; TmForce {}
                      ; TmReturn {}
                      ; TmPrimBinOp {}; TmPrimUnOp {}
                      ; TmPrintInt {}
                      ; TmPrintDouble {}) = polyRecTmM deadLetElimination tm
deadLetElimination (TmLam p tm)           = TmLam p <$> without (paramName p) (deadLetElimination tm)
deadLetElimination (TmTo tm0 x tm1)       = liftA2 (`TmTo` x) (deadLetElimination tm0) (without x $ deadLetElimination tm1)
deadLetElimination (TmLet x tm0 tm1)      = do
  (tm1', withX) <- without x $ listens (x `Set.member`) $ deadLetElimination tm1
  if withX
    then flip (TmLet x) tm1' <$> deadLetElimination tm0
    else pure tm1'
deadLetElimination (TmRec p tm)           = TmRec p <$> without (paramName p) (deadLetElimination tm)

without :: Ident -> WithFreeVars a -> WithFreeVars a
without x = censor (Set.delete x)
