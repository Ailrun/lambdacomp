{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module LambdaComp.CBPV.ToAM where

import Control.Applicative         (Applicative (liftA2))
import Control.Monad.Reader        (MonadReader (local), Reader, asks)
import Control.Monad.Writer.Strict (MonadWriter (tell), WriterT (WriterT, runWriterT), lift)
import Data.Bifunctor              (Bifunctor (first))
import Data.List                   (elemIndex)
import Data.Semigroup              (Dual (Dual))
import Data.Set                    qualified as Set

import LambdaComp.AM.Syntax
import LambdaComp.CBPV.Syntax
import LambdaComp.FreshName        (FreshNameT, freshNameOf)

type WithClosure = FreshNameT (Reader [Ident])

class ToAM a where
  type AMData a
  toAM :: a -> WithClosure (AMData a)

instance ToAM (Tm 'Val) where
  type AMData (Tm 'Val) = (Value, Dual [CodeSection])

  toAM :: Tm 'Val -> WithClosure (AMData (Tm 'Val))
  toAM (TmVar x)    = runWriterT $ VaAddr <$> getVar x
  toAM TmUnit       = runWriterT $ pure VaUnit
  toAM (TmInt n)    = runWriterT $ pure $ VaInt n
  toAM (TmDouble d) = runWriterT $ pure $ VaDouble d
  toAM (TmThunk tm) = runWriterT $ do
    thunkCode <- WriterT $ local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf "sys_thunk"
    tell $ Dual [ThunkCodeSection {..}]
    VaThunk thunkCodeSectionName <$> traverse getVar thunkEnvVars
    where
      thunkEnvVars = Set.toList thunkEnv
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm

instance ToAM (Tm 'Com) where
  type AMData (Tm 'Com) = (Code, Dual [CodeSection])

  toAM :: Tm 'Com -> WithClosure (AMData (Tm 'Com))
  toAM (TmLam x tm) = first (IPop (toVarAddr x) :) <$> toAM tm
  toAM (tmf `TmApp` tma) = runWriterT $ liftA2 (:) (IPush <$> WriterT (toAM tma)) (WriterT $ toAM tmf)
  toAM (TmForce tm) = runWriterT $ do
    thunk <- WriterT $ toAM tm
    pure [IJump thunk]
  toAM (TmReturn tm) = runWriterT $ do
    val <- WriterT $ toAM tm
    pure [IReturn val]
  toAM (TmThen tm0 x tm1) = runWriterT $ do
    code0 <- WriterT $ toAM tm0
    code1 <- WriterT $ toAM tm1
    pure (IScope : code0 <> (IReceive (toVarAddr x) : code1))
  toAM (TmLet x tm0 tm1) = runWriterT $ do
    val0 <- WriterT $ toAM tm0
    code1 <- WriterT $ toAM tm1
    pure (IAssign (toVarAddr x) val0 : code1)
  toAM (TmPrintInt tm0 tm1) = runWriterT $ do
    val0 <- WriterT $ toAM tm0
    code1 <- WriterT $ toAM tm1
    pure (IPrint val0 : code1)
  toAM (TmRec x tm) = runWriterT $ do
    thunkCode <- WriterT $ local (const thunkEnvVars) $ toAM tm
    thunkCodeSectionName <- lift $ freshNameOf "sys_thunk"
    tell $ Dual [ThunkCodeSection {..}]
    initCode <- IAssign xVar . VaThunk thunkCodeSectionName <$> traverse getVar thunkEnvVars
    pure [initCode, IJump (VaAddr xVar)]
    where
      xVar = toVarAddr x
      thunkEnvVars = Set.toList thunkEnv
      thunkEnvSize = Set.size thunkEnv
      thunkEnv = freeVarOfTm tm

getVar :: Ident -> WriterT (Dual [CodeSection]) WithClosure Addr
getVar x = do
  mayInd <- asks (elemIndex x)
  pure $ case mayInd of
      Just i  -> ALocalEnv i
      Nothing -> toVarAddr x

toVarAddr :: Ident -> Addr
toVarAddr = AIdent . ("var_" <>)
