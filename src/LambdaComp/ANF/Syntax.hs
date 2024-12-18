module LambdaComp.ANF.Syntax where

import LambdaComp.Syntax

data AVal where
  AValVar :: Ident -> AVal
  AValCtr :: Ident -> [AVal] -> AVal
  ATmVaInt :: Int -> AVal
  AValDouble :: Double -> AVal
  AValLam :: [Ident] -> [(Ident, Tp)] -> ATm -> AVal
  deriving stock (Eq, Ord, Show, Read)

data ATm where
  ATmAnn :: ATm -> Tp -> ATm
  ATmAVal :: AVal -> ATm
  ATmLet :: (Ident, Tp) -> AVal -> [Tp] -> [AVal] -> ATm -> ATm
  deriving stock (Eq, Ord, Show, Read)
