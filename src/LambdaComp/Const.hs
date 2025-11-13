module LambdaComp.Const
  ( module LambdaComp.Const
  ) where

data TpConst where
  TpCUnit   :: TpConst
  TpCBool   :: TpConst
  TpCInt    :: TpConst
  TpCDouble :: TpConst
  deriving stock (Eq, Ord, Show)

data TmConst where
  TmCUnit        :: TmConst
  TmCTrue        :: TmConst
  TmCFalse       :: TmConst
  TmCInt         :: !Int -> TmConst
  TmCDouble      :: !Double -> TmConst
  deriving stock (Eq, Ord, Show)

inferConst :: TmConst -> TpConst
inferConst TmCUnit       = TpCUnit
inferConst TmCTrue       = TpCBool
inferConst TmCFalse      = TpCBool
inferConst (TmCInt _)    = TpCInt
inferConst (TmCDouble _) = TpCDouble
