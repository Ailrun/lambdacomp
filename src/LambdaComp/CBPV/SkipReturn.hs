{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module LambdaComp.CBPV.SkipReturn where

import LambdaComp.CBPV.Syntax

topSkipReturn :: Tm 'Com -> Tm 'Com
topSkipReturn = skipReturn

skipReturn :: Tm c -> Tm c
skipReturn tm@(TmVar _)             = tm
skipReturn tm@TmUnit                = tm
skipReturn tm@(TmInt _)             = tm
skipReturn tm@(TmDouble _)          = tm
skipReturn (TmThunk tm)             = TmThunk $ skipReturn tm
skipReturn (TmForce tm)             = TmForce $ skipReturn tm
skipReturn (TmLam x tm)             = TmLam x $ skipReturn tm
skipReturn (tmf `TmApp` tma)        = skipReturn tmf `TmApp` skipReturn tma
skipReturn (TmReturn tm)            = TmReturn $ skipReturn tm
skipReturn (TmThen tm0 x tm1) =
  case skipReturn tm0 of
    TmReturn tm0' -> TmLet x tm0' (skipReturn tm1)
    tm0'          -> TmThen tm0' x (skipReturn tm1)
skipReturn (TmLet x tm0 tm1)    = TmLet x (skipReturn tm0) (skipReturn tm1)
skipReturn (TmPrintInt tm0 tm1) = TmPrintInt (skipReturn tm0) (skipReturn tm1)
skipReturn (TmRec f tm)         = TmRec f (skipReturn tm)
