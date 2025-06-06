module LambdaComp.AM.Syntax
  ( module LambdaComp.AM.Syntax
  , module LambdaComp.Ident
  , module LambdaComp.PrimOp
  ) where

import Data.String (IsString (..))
import Data.Vector (Vector)

import LambdaComp.Ident
import LambdaComp.PrimOp (PrimOp (..), PrimOpArity (..))

data Addr where
  AIdent    :: !Ident -> Addr
  ALocalEnv :: !Int -> Addr
  deriving stock (Eq, Show)

instance IsString Addr where
  fromString = AIdent . fromString

data Value where
  VaUnit   :: Value
  VaBool   :: !Bool -> Value
  VaInt    :: !Int -> Value
  VaDouble :: !Double -> Value
  VaThunk  :: !Ident -> !(Vector Addr) -> Value
  VaAddr   :: !Addr -> Value
  deriving stock Show

instance IsString Value where
  fromString = VaAddr . fromString

data Inst where
  IDefine      :: !Ident -> Inst
  IScope       :: Inst
  IPush        :: !Value -> Inst
  IPop         :: !Addr -> Inst
  IAssign      :: !Addr -> !Value -> Inst
  IJump        :: !Int -> Inst
  ICondJump    :: !Value -> !Int -> Inst
  ICall        :: !Value -> Inst
  ISetReturn   :: !Value -> Inst
  IReceive     :: !Addr -> Inst
  IRecAssign   :: !Addr -> !Ident -> !(Vector Addr) -> Inst
  IPrimBinOp   :: !(PrimOp Binary) -> Inst
  IPrimUnOp    :: !(PrimOp Unary) -> Inst
  IPrintInt    :: !Value -> Inst
  IPrintDouble :: !Value -> Inst
  IExit        :: Inst
  IEndScope    :: Inst
  deriving stock Show

type Code = Vector Inst

data CodeSection where
  ThunkCodeSection :: { thunkCodeSectionName :: !Ident, thunkCode :: !Code, thunkEnvSize :: !Int } -> CodeSection
  TmDefCodeSection :: { tmDefCode :: !Code } -> CodeSection
  deriving stock Show
