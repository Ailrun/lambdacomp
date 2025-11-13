{-# LANGUAGE TypeData     #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaComp.PrimOp
  ( module LambdaComp.PrimOp
  ) where

type data PrimOpArity where
  Unary, Binary :: PrimOpArity

type role PrimOp nominal
data PrimOp (a :: PrimOpArity) where
  PrimIAdd,
     PrimISub,
     PrimIMul,
     PrimIDiv,
     PrimIMod       :: PrimOp Binary
  PrimINeg          :: PrimOp Unary

  PrimIEq,
     PrimINEq,
     PrimILt,
     PrimILe,
     PrimIGt,
     PrimIGe        :: PrimOp Binary

  PrimIToD          :: PrimOp Unary

  PrimDAdd,
     PrimDSub,
     PrimDMul,
     PrimDDiv       :: PrimOp Binary
  PrimDNeg          :: PrimOp Unary

  PrimDEq,
     PrimDNEq,
     PrimDLt,
     PrimDLe,
     PrimDGt,
     PrimDGe        :: PrimOp Binary

  PrimDToI          :: PrimOp Unary

  PrimBNot          :: PrimOp Unary

  PrimBAnd, PrimBOr :: PrimOp Binary

deriving stock instance Eq (PrimOp a)
deriving stock instance Ord (PrimOp a)
deriving stock instance Show (PrimOp a)

-- type family PrimOpArgs (a :: PrimOpArity) tp where
--   PrimOpArgs Binary tp = (tp, tp)
--   PrimOpArgs Unary tp = tp

type role PrimOpTypeBase representational
data PrimOpTypeBase tp where
  PrimOpTypeBase :: { boolTp :: tp, intTp :: tp, doubleTp :: tp } -> PrimOpTypeBase tp

class PrimOpArgsClass (a :: PrimOpArity) where
  type PrimOpArgs (a :: PrimOpArity) tp

  getPrimOpType :: PrimOp a -> PrimOpTypeBase tp -> (PrimOpArgs a tp, tp)

instance PrimOpArgsClass Binary where
  type PrimOpArgs Binary tp = (tp, tp)

  getPrimOpType :: PrimOp Binary -> PrimOpTypeBase tp -> (PrimOpArgs Binary tp, tp)
  getPrimOpType (PrimIAdd
                ; PrimISub
                ; PrimIMul
                ; PrimIDiv
                ; PrimIMod) PrimOpTypeBase {..} = ((intTp , intTp), intTp)
  getPrimOpType (PrimIEq
                ; PrimINEq
                ; PrimILt
                ; PrimILe
                ; PrimIGt
                ; PrimIGe)  PrimOpTypeBase {..} = ((intTp, intTp), boolTp)
  getPrimOpType (PrimDAdd
                ; PrimDSub
                ; PrimDMul
                ; PrimDDiv) PrimOpTypeBase {..} = ((doubleTp, doubleTp), doubleTp)
  getPrimOpType (PrimDEq
                ; PrimDNEq
                ; PrimDLt
                ; PrimDLe
                ; PrimDGt
                ; PrimDGe)  PrimOpTypeBase {..} = ((doubleTp, doubleTp), boolTp)
  getPrimOpType (PrimBAnd
                ; PrimBOr)  PrimOpTypeBase {..} = ((boolTp, boolTp), boolTp)

instance PrimOpArgsClass Unary where
  type PrimOpArgs Unary tp = tp

  getPrimOpType :: PrimOp Unary -> PrimOpTypeBase tp -> (PrimOpArgs Unary tp, tp)
  getPrimOpType PrimINeg PrimOpTypeBase {..} = (intTp, intTp)
  getPrimOpType PrimIToD PrimOpTypeBase {..} = (intTp, doubleTp)
  getPrimOpType PrimDNeg PrimOpTypeBase {..} = (doubleTp, doubleTp)
  getPrimOpType PrimDToI PrimOpTypeBase {..} = (doubleTp, intTp)
  getPrimOpType PrimBNot PrimOpTypeBase {..} = (boolTp, boolTp)
