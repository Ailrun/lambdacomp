{-# LANGUAGE TypeData #-}
module LambdaComp.Binder
  ( module LambdaComp.Binder
  ) where

import LambdaComp.Ident (Ident)

type data BinderType where
  BTUntyped, BTTyped :: BinderType

-- | Generic Binder
--
-- Depending on @t :: BinderType@, this can be
-- either a typed binder or untyped binder.
type role Binder nominal nominal nominal
data Binder (t :: BinderType) tp tm where
  BUntyped :: !Ident -> tm -> Binder BTUntyped tp tm
  BTyped   :: !(Param tp) -> tm -> Binder BTTyped tp tm
deriving stock instance (Eq tp, Eq tm) => Eq (Binder t tp tm)
deriving stock instance (Ord tp, Ord tm) => Ord (Binder t tp tm)
deriving stock instance (Show tp, Show tm) => Show (Binder t tp tm)

type role Param nominal
data Param tp where
  Param :: { paramName :: !Ident, paramType :: !tp } -> Param tp
  deriving stock (Eq, Ord, Show)

getBoundVar :: Binder t tp tm -> Ident
getBoundVar (BUntyped x _) = x
getBoundVar (BTyped p _)   = paramName p

-- | Lens-like transformer for the body (term part) of a binder
lensBinderBody :: (Functor f) => (tm -> f tm) -> Binder t tp tm -> f (Binder t tp tm)
lensBinderBody f (BUntyped x tm) = BUntyped x <$> f tm
lensBinderBody f (BTyped p tm)   = BTyped p <$> f tm

-- | Getter of the body (term part) of a binder
--
-- We can obtain this from the 'lensBinderBody',
-- but we manually define this for better performance
getBinderBody :: Binder t tp tm -> tm
getBinderBody (BUntyped _ tm) = tm
getBinderBody (BTyped _ tm)   = tm

-- | Mapping of the body (term part) of a binder
--
-- We can obtain this from the 'lensBinderBody',
-- but we manually define this for better performance
mapBinderBody :: (tm -> tm) -> Binder t tp tm -> Binder t tp tm
mapBinderBody f (BUntyped x tm) = BUntyped x $ f tm
mapBinderBody f (BTyped p tm)   = BTyped p $ f tm
