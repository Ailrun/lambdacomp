{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBPV.ToAM
  ( runToAM
  ) where

import Control.Monad.Reader        (MonadReader (local), Reader, asks, runReader)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (runWriterT), lift)
import Data.List                   (elemIndex)
import Data.Map.Strict             qualified as Map
import Data.Set                    qualified as Set
import Data.Vector                 qualified as Vector

import LambdaComp.AM.Syntax
import LambdaComp.CBPV.Syntax
import LambdaComp.FreshName   (FreshNameT, freshNameOf, runFreshNameT)

runToAM :: Program -> [CodeSection]
runToAM = uncurry (<>) . (`runReader` []) . runFreshNameT . runWriterT . toAM

type WithAMInfo = WriterT [CodeSection] (FreshNameT (Reader [Ident]))

class ToAM a where
  type AMData a
  toAM :: a -> WithAMInfo (AMData a)

instance ToAM Program where
  type AMData Program = [CodeSection]

  toAM :: Program -> WithAMInfo (AMData Program)
  toAM prog =
    if "u_main" `Map.member` prog
    then traverse toAM $ Map.toList prog
    else error "No main function is given!"

instance ToAM (Ident, Tm Val) where
  type AMData (Ident, Tm Val) = CodeSection

  toAM :: (Ident, Tm Val) -> WithAMInfo (AMData (Ident, Tm Val))
  toAM (tmDefName, tmDefBody) = TmDefCodeSection (toVarIdent tmDefName) <$> toAM tmDefBody

instance ToAM (Tm Val) where
  type AMData (Tm Val) = Value

  toAM :: Tm Val -> WithAMInfo (AMData (Tm Val))
  toAM (TmVar x)    = VaAddr <$> getVar x
  toAM TmUnit       = pure VaUnit
  toAM TmTrue       = pure $ VaBool True
  toAM TmFalse      = pure $ VaBool False
  toAM (TmInt n)    = pure $ VaInt n
  toAM (TmDouble d) = pure $ VaDouble d
  toAM (TmThunk tm) = do
    thunkCode <- fmap (<> [IExit]) . local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf "sys_thunk"
    tell [ThunkCodeSection {..}]
    VaThunk thunkCodeSectionName . Vector.fromList <$> traverse getVar thunkEnvVars
    where
      thunkEnvVars = Set.toList thunkEnv
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm

instance ToAM (Tm Com) where
  type AMData (Tm Com) = Code

  toAM :: Tm Com -> WithAMInfo (AMData (Tm Com))
  toAM (TmIf tm0 tm1 tm2) = do
    val0 <- toAM tm0
    code1 <- toAM tm1
    code2 <- toAM tm2
    pure $ Vector.cons (ICondJump val0 (1 + length code1)) code1 <> Vector.cons (IJump (length code2)) code2
  toAM (TmLam p tm) = Vector.cons (IPop (toVarAddr (paramName p))) <$> toAM tm
  toAM (tmf `TmApp` tma) = liftA2 Vector.cons (IPush <$> toAM tma) (toAM tmf)
  toAM (TmForce tm) = pure . ICall <$> toAM tm
  toAM (TmReturn tm) = pure . ISetReturn <$> toAM tm
  toAM (TmTo tm0 x tm1) = do
    code0 <- toAM tm0
    code1 <- toAM tm1
    pure ([IScope] <> code0 <> [IEndScope, IReceive (toVarAddr x)] <> code1)
  toAM (TmLet x tm0 tm1) = liftA2 Vector.cons (IAssign (toVarAddr x) <$> toAM tm0) (toAM tm1)
  toAM (TmPrimBinOp op tm0 tm1) = do
    val0 <- toAM tm0
    val1 <- toAM tm1
    pure [IPush val0, IPush val1, IPrimBinOp op]
  toAM (TmPrimUnOp op tm) = do
    val <- toAM tm
    pure [IPush val, IPrimUnOp op]
  toAM (TmPrintInt tm0 tm1) = liftA2 Vector.cons (IPrintInt <$> toAM tm0) (toAM tm1)
  toAM (TmPrintDouble tm0 tm1) = liftA2 Vector.cons (IPrintDouble <$> toAM tm0) (toAM tm1)
  toAM (TmRec p tm) = do
    thunkCode <- fmap (<> [IExit]) . local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf "sys_thunk"
    tell [ThunkCodeSection {..}]
    initCode <- IRecAssign xVar thunkCodeSectionName . Vector.fromList <$> traverse getVar thunkEnvVars
    pure [initCode, ICall (VaAddr xVar)]
    where
      xVar = toVarAddr (paramName p)
      thunkEnvVars = Set.toList thunkEnv
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm

getVar :: Ident -> WithAMInfo Addr
getVar x = do
  mayInd <- asks (elemIndex x)
  pure $ case mayInd of
    Just i  -> ALocalEnv i
    Nothing -> toVarAddr x

toVarAddr :: Ident -> Addr
toVarAddr = AIdent . toVarIdent

toVarIdent :: Ident -> Ident
toVarIdent = ("var_" <>)
