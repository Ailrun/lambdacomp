module LambdaComp.Syntax
  ( module LambdaComp.Syntax
  , module LambdaComp.Ident
  ) where

import Data.String (IsString (fromString))

import LambdaComp.Ident

data Tp where
  TpUnit   :: Tp
  TpBool   :: Tp
  TpInt    :: Tp
  TpDouble :: Tp
  TpFun    :: Tp -> Tp -> Tp
  deriving stock (Eq, Ord, Show, Read)

data Tm where
  TmAnn      :: Tm -> Tp -> Tm
  TmVar      :: !Ident -> Tm
  TmUnit     :: Tm
  TmTrue     :: Tm
  TmFalse    :: Tm
  TmIf       :: Tm -> Tm -> Tm -> Tm
  TmInt      :: !Int -> Tm
  TmDouble   :: !Double -> Tm
  TmLam      :: !Ident -> Tm -> Tm
  TmApp      :: Tm -> Tm -> Tm
  TmPrintInt :: Tm -> Tm -> Tm
  TmRec      :: !Ident -> Tm -> Tm
  deriving stock (Eq, Ord, Show, Read)

instance IsString Tm where
  fromString = TmVar . fromString
