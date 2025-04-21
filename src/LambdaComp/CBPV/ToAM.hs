{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBPV.ToAM where

import Control.Applicative         (Applicative (liftA2))
import Control.Monad.Reader        (MonadReader (local), Reader, asks, runReader)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (WriterT, runWriterT), lift)
import Data.Array                  qualified as Array
import Data.Bifunctor              (Bifunctor (first))
import Data.List                   (elemIndex)
import Data.Set                    qualified as Set

import LambdaComp.AM.Syntax
import LambdaComp.CBPV.Syntax
import LambdaComp.FreshName        (FreshNameT, freshNameOf, runFreshNameT)

type WithClosure = FreshNameT (Reader [Ident])

runToAM :: Tm 'Com -> (Code, [CodeSection])
runToAM tm = (`runReader` []) . runFreshNameT $ toAM tm

class ToAM a where
  type AMData a
  toAM :: a -> WithClosure (AMData a)

instance ToAM (Tm 'Val) where
  type AMData (Tm 'Val) = (Value, [CodeSection])

  toAM :: Tm 'Val -> WithClosure (AMData (Tm 'Val))
  toAM (TmVar x)    = runWriterT $ VaAddr <$> getVar x
  toAM TmUnit       = runWriterT $ pure VaUnit
  toAM (TmInt n)    = runWriterT $ pure $ VaInt n
  toAM (TmDouble d) = runWriterT $ pure $ VaDouble d
  toAM (TmThunk tm) = runWriterT $ do
    thunkCode <- WriterT $ local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf "sys_thunk"
    tell [ThunkCodeSection {..}]
    VaThunk thunkCodeSectionName <$> traverse getVar thunkEnvVars
    where
      thunkEnvVars = Set.toList thunkEnv
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm

instance ToAM (Tm 'Com) where
  type AMData (Tm 'Com) = (Code, [CodeSection])

  toAM :: Tm 'Com -> WithClosure (AMData (Tm 'Com))
  toAM tm = runWriterT $ do
    insts <- WriterT $ toAMHelper tm
    pure $ Array.listArray (0, length insts) (insts <> [IExit])

toAMHelper :: Tm 'Com -> WithClosure ([Inst], [CodeSection])
toAMHelper (TmLam x tm) = first (IPop (toVarAddr x) :) <$> toAMHelper tm
toAMHelper (tmf `TmApp` tma) = runWriterT $ liftA2 (:) (IPush <$> WriterT (toAM tma)) (WriterT $ toAMHelper tmf)
toAMHelper (TmForce tm) = runWriterT $ do
  thunk <- WriterT $ toAM tm
  pure [IJump thunk]
toAMHelper (TmReturn tm) = runWriterT $ do
  val <- WriterT $ toAM tm
  pure [IReturn val]
toAMHelper (TmThen tm0 x tm1) = runWriterT $ do
  code0 <- WriterT $ toAMHelper tm0
  code1 <- WriterT $ toAMHelper tm1
  pure (IScope : code0 <> (IReceive (toVarAddr x) : code1))
toAMHelper (TmLet x tm0 tm1) = runWriterT $ do
  val0 <- WriterT $ toAM tm0
  code1 <- WriterT $ toAMHelper tm1
  pure (IAssign (toVarAddr x) val0 : code1)
toAMHelper (TmPrintInt tm0 tm1) = runWriterT $ do
  val0 <- WriterT $ toAM tm0
  code1 <- WriterT $ toAMHelper tm1
  pure (IPrintInt val0 : code1)
toAMHelper (TmRec x tm) = runWriterT $ do
  thunkCode <- WriterT $ local (const thunkEnvVars) $ toAM tm
  thunkCodeSectionName <- lift $ freshNameOf "sys_thunk"
  tell [ThunkCodeSection {..}]
  initCode <- IRecAssign xVar thunkCodeSectionName <$> traverse getVar thunkEnvVars
  pure [initCode, IJump (VaAddr xVar)]
  where
    xVar = toVarAddr x
    thunkEnvVars = Set.toList thunkEnv
    thunkEnvSize = Set.size thunkEnv
    thunkEnv = freeVarOfTm tm

getVar :: Ident -> WriterT [CodeSection] WithClosure Addr
getVar x = do
  mayInd <- asks (elemIndex x)
  pure $ case mayInd of
      Just i  -> ALocalEnv i
      Nothing -> toVarAddr x

toVarAddr :: Ident -> Addr
toVarAddr = AIdent . ("var_" <>)
