{-# LANGUAGE DefaultSignatures #-}
module Control.Monad.FreshName.Class
  ( MonadFreshName(..)

  , freshNameOf
  , freshNamesOf
  ) where

import Control.Monad.Except        (ExceptT)
import Control.Monad.Identity      (IdentityT)
import Control.Monad.Reader        (ReaderT)
import Control.Monad.RWS.CPS       qualified as CRWS
import Control.Monad.RWS.Lazy      qualified as LRWS
import Control.Monad.RWS.Strict    qualified as SRWS
import Control.Monad.State.Lazy    qualified as LS
import Control.Monad.State.Strict  qualified as SS
import Control.Monad.Trans         (MonadTrans (lift))
import Control.Monad.Trans.Accum   (AccumT)
import Control.Monad.Trans.Maybe   (MaybeT)
import Control.Monad.Writer.CPS    qualified as CW
import Control.Monad.Writer.Lazy   qualified as LW
import Control.Monad.Writer.Strict qualified as SW
import Data.String                 (IsString)

class (Monad m) => MonadFreshName m where
  genFreshNameIndex :: (IsString s) => m s
  default
    genFreshNameIndex :: (m ~ t n, MonadTrans t, MonadFreshName n, IsString s) => m s
  genFreshNameIndex = lift genFreshNameIndex

freshNameOf :: (MonadFreshName m, Semigroup s, IsString s) => s -> m s
freshNameOf stem = ((stem <> "_") <>) <$> genFreshNameIndex

freshNamesOf :: (MonadFreshName m, Semigroup s, IsString s, Functor f) => f s -> m (f s)
freshNamesOf stems = do
  idx <- genFreshNameIndex
  pure $ fmap (<> ("_" <> idx)) stems

instance (MonadFreshName m) => MonadFreshName (ExceptT e m)
instance (MonadFreshName m) => MonadFreshName (IdentityT m)
instance (MonadFreshName m) => MonadFreshName (ReaderT r m)
instance (Monoid w, MonadFreshName m) => MonadFreshName (LRWS.RWST r w s m)
instance (Monoid w, MonadFreshName m) => MonadFreshName (SRWS.RWST r w s m)
instance (Monoid w, MonadFreshName m) => MonadFreshName (CRWS.RWST r w s m)
instance (MonadFreshName m) => MonadFreshName (LS.StateT s m)
instance (MonadFreshName m) => MonadFreshName (SS.StateT s m)
instance (Monoid a, MonadFreshName m) => MonadFreshName (AccumT a m)
instance (MonadFreshName m) => MonadFreshName (MaybeT m)
instance (Monoid w, MonadFreshName m) => MonadFreshName (LW.WriterT w m)
instance (Monoid w, MonadFreshName m) => MonadFreshName (SW.WriterT w m)
instance (Monoid w, MonadFreshName m) => MonadFreshName (CW.WriterT w m)
