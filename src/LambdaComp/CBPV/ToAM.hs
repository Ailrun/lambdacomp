{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}
module LambdaComp.CBPV.ToAM
  ( runToAM
  ) where

import Control.Monad.FreshName     (FreshNameT, freshNameOf, runFreshNameT)
import Control.Monad.Reader        (MonadReader (local), Reader, asks, runReader)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (runWriterT), lift)
import Data.List                   (elemIndex)
import Data.Set                    qualified as Set
import Data.Vector                 qualified as Vector

import LambdaComp.AM.Syntax
import LambdaComp.Binder      (getBinderBody, getBoundVar)
import LambdaComp.CBPV.Syntax

runToAM :: Program -> [CodeSection]
runToAM = uncurry (<>) . (`runReader` []) . runFreshNameT . runWriterT . toAM

type WithAMInfo = WriterT [CodeSection] (FreshNameT (Reader [Ident]))

class ToAM a where
  type AMData a
  toAM :: a -> WithAMInfo (AMData a)

instance ToAM Program where
  type AMData Program = [CodeSection]

  toAM :: Program -> WithAMInfo (AMData Program)
  toAM = traverse toAM

instance ToAM Top where
  type AMData Top = CodeSection

  toAM :: Top -> WithAMInfo (AMData Top)
  toAM TopTmDef {..} = TmDefCodeSection . (<> [IDefine (toGlobalIdent tmDefName)]) <$> toAM tmDefBody

instance ToAM (Tm Val) where
  type AMData (Tm Val) = Value

  toAM :: Tm Val -> WithAMInfo (AMData (Tm Val))
  toAM (TmVar x)    = VaAddr <$> getVar x
  toAM (TmGlobal x) = VaAddr <$> getGlobal x
  toAM (TmConst c)  = toAM c
  toAM (TmThunk tm) = do
    thunkCode <- fmap (<> [IExit]) . local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf $ toLowSysVar "thunk"
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
  toAM (TmLam b) = Vector.cons (IPop (toVarAddr (getBoundVar b))) <$> toAM (getBinderBody b)
  toAM (tmf `TmApp` tma) = liftA2 Vector.cons (IPush <$> toAM tma) (toAM tmf)
  toAM (TmForce tm) = pure . ICall <$> toAM tm
  toAM (TmReturn tm) = pure . ISetReturn <$> toAM tm
  toAM (TmTo tm0 b) = do
    code0 <- toAM tm0
    code1 <- toAM $ getBinderBody b
    pure ([IScope] <> code0 <> [IEndScope, IReceive . toVarAddr $ getBoundVar b] <> code1)
  toAM (TmLet tm0 b) = liftA2 Vector.cons ((IAssign . toVarAddr $ getBoundVar b) <$> toAM tm0) . toAM $ getBinderBody b
  toAM (TmPrimBinOp op tm0 tm1) = do
    val0 <- toAM tm0
    val1 <- toAM tm1
    pure [IPush val0, IPush val1, IPrimBinOp op]
  toAM (TmPrimUnOp op tm) = do
    val <- toAM tm
    pure [IPush val, IPrimUnOp op]
  toAM (TmPrintInt tm0 tm1) = liftA2 Vector.cons (IPrintInt <$> toAM tm0) (toAM tm1)
  toAM (TmPrintDouble tm0 tm1) = liftA2 Vector.cons (IPrintDouble <$> toAM tm0) (toAM tm1)
  toAM (TmRec b) = do
    thunkCode <- fmap (<> [IExit]) . local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf $ toLowSysVar "thunk"
    tell [ThunkCodeSection {..}]
    initCode <- IRecAssign xVar thunkCodeSectionName . Vector.fromList <$> traverse getVar thunkEnvVars
    pure [initCode, ICall (VaAddr xVar)]
    where
      tm = getBinderBody b
      xVar = toVarAddr $ getBoundVar b
      thunkEnvVars = Set.toList thunkEnv
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm

instance ToAM TmConst where
  type AMData TmConst = Value

  toAM :: TmConst -> WithAMInfo (AMData TmConst)
  toAM TmCUnit       = pure VaUnit
  toAM TmCTrue       = pure $ VaBool True
  toAM TmCFalse      = pure $ VaBool False
  toAM (TmCInt n)    = pure $ VaInt n
  toAM (TmCDouble d) = pure $ VaDouble d

getVar :: Ident -> WithAMInfo Addr
getVar x = do
  mayInd <- asks (elemIndex x)
  pure $ case mayInd of
    Just i  -> ALocalEnv i
    Nothing -> toVarAddr x

toVarAddr :: Ident -> Addr
toVarAddr = AIdent . toVarIdent

getGlobal :: Ident -> WithAMInfo Addr
getGlobal x = pure $ toGlobalAddr x

toGlobalAddr :: Ident -> Addr
toGlobalAddr = AIdent . toGlobalIdent
