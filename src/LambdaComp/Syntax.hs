module LambdaComp.Syntax where

import Data.String (IsString)

newtype Ident = Ident String
  deriving newtype (Eq, Ord, Show, Read, IsString)

data Tp where
  TpVar :: Ident -> Tp
  TpData :: Ident -> Tp
  TpInt :: Tp
  TpDouble :: Tp
  TpFun :: Tp -> Tp -> Tp
  TpApp :: Tp -> Tp -> Tp
  deriving stock (Eq, Ord, Show, Read)

data Tm where
  TmAnn :: Tm -> Tp -> Tm
  TmVar :: Ident -> Tm
  TmCtr :: Ident -> [Tm] -> Tm
  TmInt :: Int -> Tm
  TmDouble :: Double -> Tm
  TmTpLam :: Ident -> Tm -> Tm
  TmTpApp :: Tm -> Tp -> Tm
  TmTmLam :: (Ident, Tp) -> Tm -> Tm
  TmTmApp :: Tm -> Tm -> Tm
  TmLet :: (Ident, Tp) -> Tm -> Tm -> Tm
  TmMatch :: Tm -> [Alt] -> Tm
  deriving stock (Eq, Ord, Show, Read)

newtype Alt = Alt [(Ident, Tp, Tm)]
  deriving stock (Eq, Ord, Show, Read)
