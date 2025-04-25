{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData        #-}
{-# LANGUAGE TypeFamilies    #-}
module LambdaComp.PrimOp where

type data PrimOpArity where
  Unary  :: PrimOpArity
  Binary :: PrimOpArity

data PrimOp (a :: PrimOpArity) where
  PrimIAdd :: PrimOp Binary
  PrimISub :: PrimOp Binary
  PrimIMul :: PrimOp Binary
  PrimIDiv :: PrimOp Binary
  PrimIMod :: PrimOp Binary
  PrimINeg :: PrimOp Unary
  PrimIEq  :: PrimOp Binary
  PrimINEq :: PrimOp Binary
  PrimILt  :: PrimOp Binary
  PrimILe  :: PrimOp Binary
  PrimIGt  :: PrimOp Binary
  PrimIGe  :: PrimOp Binary
  PrimDAdd :: PrimOp Binary
  PrimDSub :: PrimOp Binary
  PrimDMul :: PrimOp Binary
  PrimDDiv :: PrimOp Binary
  PrimDNeg :: PrimOp Unary
  PrimDEq  :: PrimOp Binary
  PrimDNEq :: PrimOp Binary
  PrimDLt  :: PrimOp Binary
  PrimDLe  :: PrimOp Binary
  PrimDGt  :: PrimOp Binary
  PrimDGe  :: PrimOp Binary
  PrimBNot :: PrimOp Unary
  PrimBAnd :: PrimOp Binary
  PrimBOr  :: PrimOp Binary

deriving instance Eq (PrimOp a)
deriving instance Ord (PrimOp a)
deriving instance Show (PrimOp a)

type family PrimOpArgs (a :: PrimOpArity) tp where
  PrimOpArgs Binary tp = (tp, tp)
  PrimOpArgs Unary tp = tp

data PrimOpTypeBase tp where
  PrimOpTypeBase :: { boolTp :: tp, intTp :: tp, doubleTp :: tp } -> PrimOpTypeBase tp

getPrimOpType :: PrimOp a -> PrimOpTypeBase tp -> (PrimOpArgs a tp, tp)
getPrimOpType PrimIAdd PrimOpTypeBase {..} = ((intTp , intTp), intTp)
getPrimOpType PrimISub PrimOpTypeBase {..} = ((intTp , intTp), intTp)
getPrimOpType PrimIMul PrimOpTypeBase {..} = ((intTp , intTp), intTp)
getPrimOpType PrimIDiv PrimOpTypeBase {..} = ((intTp , intTp), intTp)
getPrimOpType PrimIMod PrimOpTypeBase {..} = ((intTp , intTp), intTp)
getPrimOpType PrimINeg PrimOpTypeBase {..} = (intTp, intTp)
getPrimOpType PrimIEq  PrimOpTypeBase {..} = ((intTp, intTp), boolTp)
getPrimOpType PrimINEq PrimOpTypeBase {..} = ((intTp, intTp), boolTp)
getPrimOpType PrimILt  PrimOpTypeBase {..} = ((intTp, intTp), boolTp)
getPrimOpType PrimILe  PrimOpTypeBase {..} = ((intTp, intTp), boolTp)
getPrimOpType PrimIGt  PrimOpTypeBase {..} = ((intTp, intTp), boolTp)
getPrimOpType PrimIGe  PrimOpTypeBase {..} = ((intTp, intTp), boolTp)
getPrimOpType PrimDAdd PrimOpTypeBase {..} = ((doubleTp, doubleTp), doubleTp)
getPrimOpType PrimDSub PrimOpTypeBase {..} = ((doubleTp, doubleTp), doubleTp)
getPrimOpType PrimDMul PrimOpTypeBase {..} = ((doubleTp, doubleTp), doubleTp)
getPrimOpType PrimDDiv PrimOpTypeBase {..} = ((doubleTp, doubleTp), doubleTp)
getPrimOpType PrimDNeg PrimOpTypeBase {..} = (doubleTp, doubleTp)
getPrimOpType PrimDEq  PrimOpTypeBase {..} = ((doubleTp, doubleTp), boolTp)
getPrimOpType PrimDNEq PrimOpTypeBase {..} = ((doubleTp, doubleTp), boolTp)
getPrimOpType PrimDLt  PrimOpTypeBase {..} = ((doubleTp, doubleTp), boolTp)
getPrimOpType PrimDLe  PrimOpTypeBase {..} = ((doubleTp, doubleTp), boolTp)
getPrimOpType PrimDGt  PrimOpTypeBase {..} = ((doubleTp, doubleTp), boolTp)
getPrimOpType PrimDGe  PrimOpTypeBase {..} = ((doubleTp, doubleTp), boolTp)
getPrimOpType PrimBNot PrimOpTypeBase {..} = (boolTp, boolTp)
getPrimOpType PrimBAnd PrimOpTypeBase {..} = ((boolTp, boolTp), boolTp)
getPrimOpType PrimBOr  PrimOpTypeBase {..} = ((boolTp, boolTp), boolTp)
