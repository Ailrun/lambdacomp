module LambdaComp.Syntax
  ( module LambdaComp.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Data.String (IsString (..))

import LambdaComp.Ident
import LambdaComp.PrimOp (PrimOp(..), PrimOpArity (..))

data Tp where
  TpUnit   :: Tp
  TpBool   :: Tp
  TpInt    :: Tp
  TpDouble :: Tp
  TpFun    :: Tp -> Tp -> Tp
  deriving stock (Eq, Ord, Show)

data Tm where
  TmAnn         :: Tm -> Tp -> Tm
  TmVar         :: !Ident -> Tm
  TmUnit        :: Tm
  TmTrue        :: Tm
  TmFalse       :: Tm
  TmInt         :: !Int -> Tm
  TmDouble      :: !Double -> Tm
  TmIf          :: Tm -> Tm -> Tm -> Tm
  TmLam         :: !Ident -> Tm -> Tm
  TmApp         :: Tm -> Tm -> Tm
  TmPrimBinOp   :: !(PrimOp Binary) -> Tm -> Tm -> Tm
  TmPrimUnOp    :: !(PrimOp Unary) -> Tm -> Tm
  TmPrintInt    :: Tm -> Tm -> Tm
  TmRec         :: !Ident -> Tm -> Tm

deriving stock instance Eq Tm
deriving stock instance Ord Tm
deriving stock instance Show Tm

instance IsString Tm where
  fromString = TmVar . fromString
