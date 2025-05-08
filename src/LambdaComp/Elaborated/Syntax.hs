module LambdaComp.Elaborated.Syntax
  ( module LambdaComp.Elaborated.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Data.String (IsString (..))

import LambdaComp.Ident
import LambdaComp.PrimOp (PrimOp (..), PrimOpArity (..))

type Program = [Top]

data Top
  = TopTmDef
    { tmDefName :: Ident
    , tmDefBody :: Tm
    }
  deriving stock (Eq, Ord, Show)

data Tp where
  TpUnit   :: Tp
  TpBool   :: Tp
  TpInt    :: Tp
  TpDouble :: Tp
  TpFun    :: ![Tp] -> Tp -> Tp
  deriving stock (Eq, Ord, Show)

data Param where
  Param :: { paramName :: !Ident, paramType :: !Tp } -> Param
  deriving stock (Eq, Ord, Show)

data Tm where
  TmVar         :: !Ident -> Tm
  TmUnit        :: Tm
  TmTrue        :: Tm
  TmFalse       :: Tm
  TmInt         :: !Int -> Tm
  TmDouble      :: !Double -> Tm
  TmIf          :: Tm -> Tm -> Tm -> Tm
  TmLam         :: ![Param] -> Tm -> Tm
  TmApp         :: Tm -> ![Tm] -> Tm
  TmPrimBinOp   :: !(PrimOp Binary) -> Tm -> Tm -> Tm
  TmPrimUnOp    :: !(PrimOp Unary) -> Tm -> Tm
  TmPrintInt    :: Tm -> Tm -> Tm
  TmRec         :: !Ident -> !Tp -> Tm -> Tm
  deriving stock (Eq, Ord, Show)

instance IsString Tm where
  fromString = TmVar . fromString
