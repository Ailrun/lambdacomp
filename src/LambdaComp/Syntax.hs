module LambdaComp.Syntax where

import Data.String (IsString)

newtype Ident = Ident String
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup)

data Tp where
  TpUnit :: Tp
  TpInt :: Tp
  TpDouble :: Tp
  TpFun :: Tp -> Tp -> Tp
  deriving stock (Eq, Ord, Show, Read)

data Tm where
  TmAnn :: Tm -> Tp -> Tm
  TmVar :: Ident -> Tm
  TmUnit :: Tm
  TmInt :: Int -> Tm
  TmDouble :: Double -> Tm
  TmLam :: Ident -> Tm -> Tm
  TmApp :: Tm -> Tm -> Tm
  TmPrint :: Tm -> Tm -> Tm
  TmRec :: Ident -> Tm -> Tm
  deriving stock (Eq, Ord, Show, Read)
