-- | A Definition of Tagged Binary Tree
--
module Data.TaggedTree
  ( module Data.TaggedTree
  ) where

type role TaggedTree representational representational
data TaggedTree tag a where
  Leaf   :: a -> TaggedTree tag a
  Branch :: tag -> TaggedTree tag a -> TaggedTree tag a -> TaggedTree tag a
  deriving stock Functor

instance Applicative (TaggedTree tag) where
  pure = Leaf
  ft <*> at = ft >>= flip fmap at

instance Monad (TaggedTree tag) where
  Leaf a         >>= f = f a
  Branch tag l r >>= f = Branch tag (l >>= f) (r >>= f)
