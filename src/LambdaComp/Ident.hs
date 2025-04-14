module LambdaComp.Ident where

import Data.String (IsString)

newtype Ident = Ident String
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup)
