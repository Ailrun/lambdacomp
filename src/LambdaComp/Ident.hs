module LambdaComp.Ident where

import Data.String (IsString)
import Data.Text   (Text)

newtype Ident = Ident Text
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup)
