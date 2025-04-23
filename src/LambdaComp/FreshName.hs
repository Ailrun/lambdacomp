module LambdaComp.FreshName
  ( FreshNameT
  , FreshName

  , runFreshNameT
  , runFreshName

  , freshNameOf
  , freshNamesOf
  ) where

import Control.Monad.Identity     (Identity)
import Control.Monad.State.Strict (MonadState (state), StateT, evalState, evalStateT)
import Data.String                (IsString (fromString))

type FreshNameT = StateT Integer
type FreshName = FreshNameT Identity

makeFreshNameIndex :: (Monad m, Semigroup s, IsString s) => FreshNameT m s
makeFreshNameIndex = fromString . show <$> state (\i -> (i, 1 + i))

freshNameOf :: (Monad m, Semigroup s, IsString s) => s -> FreshNameT m s
freshNameOf stem = (stem <>) <$> makeFreshNameIndex

freshNamesOf :: (Monad m, Semigroup s, IsString s, Functor f) => f s -> FreshNameT m (f s)
freshNamesOf stems = do
  idx <- makeFreshNameIndex
  pure $ fmap (<> idx) stems

runFreshNameT :: Monad m => FreshNameT m a -> m a
runFreshNameT = (`evalStateT` 0)

runFreshName :: FreshName a -> a
runFreshName = (`evalState` 0)
