module LambdaComp.Ident
  ( module LambdaComp.Ident
  ) where

import Data.String   (IsString)
import Data.Text     (Text)
import Prettyprinter (Pretty)

newtype Ident = Ident Text
  deriving newtype (Eq, Ord, Show, Read, IsString, Semigroup, Pretty)

-- Variables obtained by elaboration
toExtVar :: Ident -> Ident
toExtVar = ("e_" <>)
{-# INLINE toExtVar #-}

-- Variables attached by arity analysis
toArityVar :: Ident -> Ident
toArityVar = ("aa_" <>)
{-# INLINE toArityVar #-}

-- Variables attached by arity analysis
toArityTempVar :: Ident -> Ident
toArityTempVar = (<> "_temp")
{-# INLINE toArityTempVar #-}

-- Variables obtained by CBPV translation
toCBPVVar :: Ident -> Ident
toCBPVVar = ("c_" <>)
{-# INLINE toCBPVVar #-}

-- Variables used for low-level system operations
toLowSysVar :: Ident -> Ident
toLowSysVar = ("sys_" <>)
{-# INLINE toLowSysVar #-}

-- Variables used as low-level top-level defs
toGlobalIdent :: Ident -> Ident
toGlobalIdent = ("top_" <>)
{-# INLINE toGlobalIdent #-}

-- Variables obtained by low-level translation
toVarIdent :: Ident -> Ident
toVarIdent = ("var_" <>)
{-# INLINE toVarIdent #-}
