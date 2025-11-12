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
