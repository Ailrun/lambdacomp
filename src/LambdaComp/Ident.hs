module LambdaComp.Ident
  ( module LambdaComp.Ident
  ) where

import Data.String (IsString)
import Data.Text   (Text)

newtype Ident = Ident Text
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup)

toUserVar :: Ident -> Ident
toUserVar = ("u_" <>)
{-# INLINE toUserVar #-}

toCBPVVar :: Ident -> Ident
toCBPVVar = ("c_" <>)
{-# INLINE toCBPVVar #-}

toLowSysVar :: Ident -> Ident
toLowSysVar = ("sys_" <>)
{-# INLINE toLowSysVar #-}

toGlobalIdent :: Ident -> Ident
toGlobalIdent = ("top_" <>)
{-# INLINE toGlobalIdent #-}

toVarIdent :: Ident -> Ident
toVarIdent = ("var_" <>)
{-# INLINE toVarIdent #-}
