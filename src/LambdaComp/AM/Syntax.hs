{-# LANGUAGE DataKinds #-}
module LambdaComp.AM.Syntax where

import Data.Vector      (Vector)

import LambdaComp.Ident

data Addr where
  AIdent    :: !Ident -> Addr
  ALocalEnv :: !Int -> Addr
  deriving (Eq, Show)

data Value where
  VaUnit   :: Value
  VaInt    :: !Int -> Value
  VaDouble :: !Double -> Value
  VaThunk  :: !Ident -> !(Vector Addr) -> Value
  VaAddr   :: !Addr -> Value
  deriving Show

data Inst where
  IScope     :: Inst
  IPush      :: !Value -> Inst
  IPop       :: !Addr -> Inst
  IAssign    :: !Addr -> !Value -> Inst
  IJump      :: !Value -> Inst
  IReturn    :: !Value -> Inst
  IReceive   :: !Addr -> Inst
  IRecAssign :: !Addr -> !Ident -> !(Vector Addr) -> Inst
  IPrintInt  :: !Value -> Inst
  IExit      :: Inst
  deriving Show

type Code = Vector Inst

data CodeSection
  = ThunkCodeSection
    { thunkCodeSectionName :: !Ident
    , thunkCode            :: !Code
    , thunkEnvSize         :: !Int
    }
  | MainCodeSection
    { mainCode             :: !Code
    }
  deriving Show
