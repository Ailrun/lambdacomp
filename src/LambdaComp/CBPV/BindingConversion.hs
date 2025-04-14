{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module LambdaComp.CBPV.BindingConversion where

import Control.Monad.Writer.Strict (MonadWriter (tell), Writer, runWriter)
import Data.Kind                   (Type)

import LambdaComp.CBPV.Syntax

topCommutingThen :: Tm 'Com -> Tm 'Com
topCommutingThen = uncurry commitThen . runWriter . commutingThen

topLiftingLet :: Tm 'Com -> Tm 'Com
topLiftingLet = uncurry commitLet . runWriter . liftingLet

data TmThenPrefix = TmThenPrefix (Tm 'Com) Ident

type family CommutingThen (c :: Class) (a :: Type) where
  CommutingThen 'Com a = Writer [TmThenPrefix] a
  CommutingThen 'Val a = a

commutingThen :: Tm c -> CommutingThen c (Tm c)
commutingThen tm@(TmVar _)         = tm
commutingThen tm@TmUnit            = tm
commutingThen tm@(TmInt _)         = tm
commutingThen tm@(TmDouble _)      = tm
commutingThen (TmThunk tm)         = TmThunk $ topCommutingThen tm
commutingThen (TmForce tm)         = pure . TmForce $ commutingThen tm
commutingThen (TmLam x tm)         = pure . TmLam x $ topCommutingThen tm
commutingThen (tmf `TmApp` tma)    = (`TmApp` commutingThen tma) <$> commutingThen tmf
commutingThen (TmReturn tm)        = pure . TmReturn $ commutingThen tm
commutingThen (TmThen tm0 x tm1)   = do
  tm0' <- commutingThen tm0
  tell [TmThenPrefix tm0' x]
  commutingThen tm1
commutingThen (TmLet x tm0 tm1)    = pure . TmLet x (commutingThen tm0) $ topCommutingThen tm1
commutingThen (TmPrintInt tm0 tm1) = TmPrintInt (commutingThen tm0) <$> commutingThen tm1
commutingThen (TmRec f tm)         = pure . TmRec f $ topCommutingThen tm

commitThen :: Tm 'Com -> [TmThenPrefix] -> Tm 'Com
commitThen = foldr (\(TmThenPrefix tm0 x) -> TmThen tm0 x)

data TmLetPrefix = TmLetPrefix Ident (Tm 'Val)

type family LiftingLet (c :: Class) (a :: Type) where
  LiftingLet 'Com a = Writer [TmLetPrefix] a
  LiftingLet 'Val a = a

liftingLet :: Tm c -> LiftingLet c (Tm c)
liftingLet tm@(TmVar _)         = tm
liftingLet tm@TmUnit            = tm
liftingLet tm@(TmInt _)         = tm
liftingLet tm@(TmDouble _)      = tm
liftingLet (TmThunk tm)         = TmThunk $ topLiftingLet tm
liftingLet (TmForce tm)         = pure . TmForce $ liftingLet tm
liftingLet (TmLam x tm)         = pure . TmLam x $ topLiftingLet tm
liftingLet (tmf `TmApp` tma)    = (`TmApp` liftingLet tma) <$> liftingLet tmf
liftingLet (TmReturn tm)        = pure . TmReturn $ liftingLet tm
liftingLet (TmThen tm0 x tm1)   = do
  tm0' <- liftingLet tm0
  pure . TmThen tm0' x $ topLiftingLet tm1
liftingLet (TmLet x tm0 tm1)    = do
  tell [TmLetPrefix x (liftingLet tm0)]
  liftingLet tm1
liftingLet (TmPrintInt tm0 tm1) = TmPrintInt (liftingLet tm0) <$> liftingLet tm1
liftingLet (TmRec f tm)         = pure . TmRec f $ topLiftingLet tm

commitLet :: Tm 'Com -> [TmLetPrefix] -> Tm 'Com
commitLet = foldr (\(TmLetPrefix tm0 x) -> TmLet tm0 x)
