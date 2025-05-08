module LambdaComp.External.Syntax
  ( module LambdaComp.External.Syntax
  , module LambdaComp.Elaborated.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp

  , SourcePos (..)
  , sourcePosPretty
  ) where

import Data.String     (IsString (..))
import Text.Megaparsec (SourcePos (..), mkPos, sourcePosPretty)

import LambdaComp.Elaborated.Syntax (Tp (..))
import LambdaComp.Ident
import LambdaComp.PrimOp            (PrimOp (..), PrimOpArity (..))

data SourceSpan where
  SourceSpan :: { startPos :: SourcePos, endPos :: SourcePos } -> SourceSpan
  deriving stock (Eq, Ord, Show)

type Program = [XTop]

data Top
  = TopTmDecl
    { tmDefName :: Ident
    , tmDefType :: XTp
    }
  | TopTmDef
    { tmDefName :: Ident
    , tmDefBody :: XTm
    }
  deriving stock (Eq, Ord, Show)

type XTop = (Top, SourceSpan)

type XTp = (Tp, SourceSpan)

data Param where
  Param :: { paramName :: !Ident, paramType :: !(Maybe Tp) } -> Param
  deriving stock (Eq, Ord, Show)

type XParam = (Param, SourceSpan)

data Tm where
  TmAnn         :: XTm -> XTp -> Tm
  TmVar         :: !Ident -> Tm
  TmUnit        :: Tm
  TmTrue        :: Tm
  TmFalse       :: Tm
  TmInt         :: !Int -> Tm
  TmDouble      :: !Double -> Tm
  TmIf          :: XTm -> XTm -> XTm -> Tm
  TmLam         :: ![XParam] -> XTm -> Tm
  TmApp         :: XTm -> ![XTm] -> Tm
  TmPrimBinOp   :: !(PrimOp Binary) -> XTm -> XTm -> Tm
  TmPrimUnOp    :: !(PrimOp Unary) -> XTm -> Tm
  TmPrintInt    :: XTm -> XTm -> Tm
  deriving stock (Eq, Ord, Show)

instance IsString Tm where
  fromString = TmVar . fromString

type XTm = (Tm, SourceSpan)

liftX2 :: ((a, SourceSpan) -> (b, SourceSpan) -> c) -> (a, SourceSpan) -> (b, SourceSpan) -> (c, SourceSpan)
liftX2 f op0 op1 = (f op0 op1, mergeSourceSpan (snd op0) (snd op1))

ghostSourceSpan :: SourceSpan
ghostSourceSpan = SourceSpan ghostSourcePos ghostSourcePos

ghostSourcePos :: SourcePos
ghostSourcePos = SourcePos "" (mkPos 0) (mkPos 0)

getSourceSpan :: (a, SourceSpan) -> SourceSpan
getSourceSpan = snd

mergeSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
mergeSourceSpan p q = SourceSpan (startPos p) (endPos q)
