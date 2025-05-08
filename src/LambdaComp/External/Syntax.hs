module LambdaComp.External.Syntax
  ( module LambdaComp.External.Syntax
  , module LambdaComp.Elaborated.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Data.String (IsString (..))

import LambdaComp.Elaborated.Syntax (Tp (..))
import LambdaComp.Ident
import LambdaComp.PrimOp            (PrimOp (..), PrimOpArity (..))

type Program = [Top]

data Top
  = TopTmDecl
    { tmDefName :: Ident
    , tmDefType :: Tp
    }
  | TopTmDef
    { tmDefName :: Ident
    , tmDefBody :: Tm
    }
  deriving stock (Show)

data Param where
  Param :: { paramName :: !Ident, paramType :: !(Maybe Tp) } -> Param
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
  TmLam         :: ![Param] -> Tm -> Tm
  TmApp         :: Tm -> ![Tm] -> Tm
  TmPrimBinOp   :: !(PrimOp Binary) -> Tm -> Tm -> Tm
  TmPrimUnOp    :: !(PrimOp Unary) -> Tm -> Tm
  TmPrintInt    :: Tm -> Tm -> Tm
  deriving stock (Eq, Ord, Show)

instance IsString Tm where
  fromString = TmVar . fromString
