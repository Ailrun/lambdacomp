{-# LANGUAGE OverloadedStrings #-}
module LambdaComp.Ident where

import Data.String (IsString)
import Data.Text   (Text)

newtype Ident = Ident Text
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup)

toUserVar :: Ident -> Ident
toUserVar = ("u_" <>)

toCBPVVar :: Ident -> Ident
toCBPVVar = ("c_" <>)

toLowSysVar :: Ident -> Ident
toLowSysVar = ("sys_" <>)

toGlobalIdent :: Ident -> Ident
toGlobalIdent = ("top_" <>)

toVarIdent :: Ident -> Ident
toVarIdent = ("var_" <>)

mainForCBPV :: Ident
mainForCBPV = toCBPVVar "main"
