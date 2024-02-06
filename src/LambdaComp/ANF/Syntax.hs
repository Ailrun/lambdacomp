module LambdaComp.ANF.Syntax where

import LambdaComp.Syntax

data AnfVal where
  AnfValVar :: Ident -> AnfVal
  AnfVaInt :: Int -> AnfVal
  AnfValDouble :: Double -> AnfVal
  AnfValLam :: Ident -> Tp -> Anf -> AnfVal
  deriving stock (Eq, Ord, Show, Read)

data Anf where
  AnfAnn :: Anf -> Tp -> Anf
  AnfAnfVal :: AnfVal -> Anf
  AnfLetAnfVal :: Ident -> Tp -> AnfVal -> Anf -> Anf
  AnfLetApp :: Ident -> Tp -> AnfVal -> AnfVal -> Anf -> Anf
  deriving stock (Eq, Ord, Show, Read)
