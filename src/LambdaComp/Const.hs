module LambdaComp.Const
  ( module LambdaComp.Const
  ) where

data TpConst where
  TpCUnit,
     TpCBool,
     TpCInt,
     TpCDouble :: TpConst
  deriving stock (Eq, Ord, Show)

data TmConst where
  TmCUnit,
     TmCTrue,
     TmCFalse :: TmConst
  TmCInt      :: !Int -> TmConst
  TmCDouble   :: !Double -> TmConst
  deriving stock (Eq, Ord, Show)

inferConst :: TmConst -> TpConst
inferConst TmCUnit       = TpCUnit
inferConst TmCTrue       = TpCBool
inferConst TmCFalse      = TpCBool
inferConst (TmCInt _)    = TpCInt
inferConst (TmCDouble _) = TpCDouble
