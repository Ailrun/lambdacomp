{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBPV.ToAM where

import Control.Monad.Reader        (MonadReader (local), Reader, asks, runReader)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (runWriterT), lift)
import Data.Bifunctor              (Bifunctor (first))
import Data.List                   (elemIndex)
import Data.Set                    qualified as Set
import Data.Vector                 qualified as Vector

import LambdaComp.AM.Syntax
import LambdaComp.CBPV.Syntax
import LambdaComp.FreshName   (FreshNameT, freshNameOf, runFreshNameT)

type WithAMInfo = WriterT [CodeSection] (FreshNameT (Reader [Ident]))

runToAM :: Tm Com -> [CodeSection]
runToAM tm = uncurry (:) . first MainCodeSection . (`runReader` []) . runFreshNameT . runWriterT $ toAM tm

class ToAM a where
  type AMData a
  toAM :: a -> WithAMInfo (AMData a)

instance ToAM (Tm Val) where
  type AMData (Tm Val) = Value

  toAM :: Tm Val -> WithAMInfo (AMData (Tm Val))
  toAM (TmVar x)    = VaAddr <$> getVar x
  toAM TmUnit       = pure VaUnit
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
  toAM (TmLam x tm) = ([IPop (toVarAddr x)] <>) <$> toAM tm
  toAM (tmf `TmApp` tma) = liftA2 Vector.cons (IPush <$> toAM tma) (toAM tmf)
  toAM (TmForce tm) = do
    thunk <- toAM tm
    pure [IJump thunk]
  toAM (TmReturn tm) = do
    val <- toAM tm
    pure [IReturn val]
  toAM (TmThen tm0 x tm1) = do
    code0 <- toAM tm0
    code1 <- toAM tm1
    pure ([IScope] <> code0 <> [IReceive (toVarAddr x)] <> code1)
  toAM (TmLet x tm0 tm1) = do
    val0 <- toAM tm0
    code1 <- toAM tm1
    pure ([IAssign (toVarAddr x) val0] <> code1)
  toAM (TmPrintInt tm0 tm1) = do
    val0 <- toAM tm0
    code1 <- toAM tm1
    pure ([IPrintInt val0] <> code1)
  toAM (TmRec x tm) = do
    thunkCode <- fmap (<> [IExit]) . local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf "sys_thunk"
    tell [ThunkCodeSection {..}]
    initCode <- IRecAssign xVar thunkCodeSectionName . Vector.fromList <$> traverse getVar thunkEnvVars
    pure [initCode, IJump (VaAddr xVar)]
    where
      xVar = toVarAddr x
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
toVarAddr = AIdent . ("var_" <>)
