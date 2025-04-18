module LambdaComp.FreshName where

import Control.Monad.Identity     (Identity)
import Control.Monad.State.Strict (MonadState (get), StateT, modify')
import Data.String                (IsString (fromString))

type FreshNameT = StateT Integer
type FreshName = FreshNameT Identity

freshNameOf :: (Monad m, Semigroup s, IsString s) => s -> FreshNameT m s
freshNameOf stem = do
  i <- get
  modify' (1 +)
  pure $ stem <> fromString (show i)

freshNamesOf :: (Monad m, Semigroup s, IsString s, Functor f) => f s -> FreshNameT m (f s)
freshNamesOf stems = do
  i <- get
  modify' (1 +)
  pure $ fmap (<> fromString (show i)) stems
