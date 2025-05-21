{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.FreshName
  ( FreshNameT
  , FreshName

  , runFreshNameT
  , runFreshName

  , MonadFreshName (..)
  , freshNameOf
  , freshNamesOf
  ) where

import Control.Monad.FreshName.Class (MonadFreshName (..), freshNameOf, freshNamesOf)
import Control.Monad.Identity        (Identity)
import Control.Monad.Reader.Class    (MonadReader)
import Control.Monad.State.Strict    (MonadState (state), StateT, evalState, evalStateT)
import Control.Monad.Trans           (MonadTrans (lift))
import Control.Monad.Writer.Class    (MonadWriter)
import Data.String                   (IsString (fromString))

type role FreshNameT representational nominal
newtype FreshNameT m a where
  FreshNameT :: { getStateT :: StateT Integer m a } -> FreshNameT m a
  deriving newtype (Functor, Applicative, Monad, MonadTrans)
deriving newtype instance (MonadReader r m) => MonadReader r (FreshNameT m)
deriving newtype instance (MonadWriter w m) => MonadWriter w (FreshNameT m)

runFreshNameT :: Monad m => FreshNameT m a -> m a
runFreshNameT = (`evalStateT` 0) . getStateT
{-# INLINE runFreshNameT #-}

instance (Monad m) => MonadFreshName (FreshNameT m) where
  genFreshNameIndex = FreshNameT $ state (\i -> (fromString $ show i, 1 + i))

instance (MonadState s m) => MonadState s (FreshNameT m) where
  state = lift . state

type FreshName = FreshNameT Identity

runFreshName :: FreshName a -> a
runFreshName = (`evalState` 0) . getStateT
{-# INLINE runFreshName #-}
