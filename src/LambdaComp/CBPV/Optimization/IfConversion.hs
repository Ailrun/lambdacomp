{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.Optimization.IfConversion
  ( runCommutingIf
  ) where

import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Set              qualified as Set
import Data.TaggedTree       (TaggedTree (..))

import LambdaComp.Binder      (getBoundVar, lensBinderBody)
import LambdaComp.CBPV.Syntax

runCommutingIf :: Tm Com -> Tm Com
runCommutingIf = closedCommutingIf

------------------------------------------------------------
-- lifting/commuting printInt

closedCommutingIf :: Tm Com -> Tm Com
closedCommutingIf = commitIf . commutingIf

commutingIfUnder :: Binder t -> CommutingIf Com (Binder t)
commutingIfUnder b = lensBinderBody (commitIfUnder (getBoundVar b) . commutingIf) b

type IfContext = TaggedTree (Tm Val)

class CommutingIfClass (c :: Class) where
  type CommutingIf (c :: Class) a
  commutingIf :: Tm c -> CommutingIf c (Tm c)

instance CommutingIfClass Com where
  type CommutingIf Com a = IfContext a

  commutingIf tm@(TmLam _; TmApp {}
                 ; TmForce _
                 ; TmReturn _; TmTo {}
                 ; TmLet {}
                 ; TmPrimBinOp {}; TmPrimUnOp {}
                 ; TmPrintInt {}; TmPrintDouble {}
                 ; TmRec _)      = recTmBM (Identity . commutingIf) commutingIf commutingIfUnder (pure . runIdentity) tm
  commutingIf (TmIf tm0 tm1 tm2) = Branch (commutingIf tm0) (commutingIf tm1) (commutingIf tm2)

instance CommutingIfClass Val where
  type CommutingIf Val a = a

  commutingIf = recTm commutingIf closedCommutingIf

commitIfUnder :: Ident -> CommutingIf Com (Tm Com) -> CommutingIf Com (Tm Com)
commitIfUnder _ ctx@(Leaf _)           = ctx
commitIfUnder x (Branch tm0 ctx1 ctx2)
  | x `Set.member` freeVarOfTm tm0     = pure $ TmIf tm0 (commitIf ctx1) (commitIf ctx2)
  | otherwise                          = Branch tm0 (commitIfUnder x ctx1) (commitIfUnder x ctx2)

commitIf :: CommutingIf Com (Tm Com) -> Tm Com
commitIf (Leaf tm)              = tm
commitIf (Branch tm0 ctx1 ctx2) = TmIf tm0 (commitIf ctx1) (commitIf ctx2)
