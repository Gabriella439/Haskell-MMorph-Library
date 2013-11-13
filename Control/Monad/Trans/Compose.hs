{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

{-| Composition of monad transformers. A higher-order version of
    "Data.Functor.Compose".
-}

module Control.Monad.Trans.Compose (
    ComposeT(ComposeT, getComposeT),
   ) where

import Control.Applicative (Applicative, Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable (traverse))

{- | Right-to-left composition of monad transformers.
-}
newtype ComposeT (f :: (* -> *) -> * -> *) (g :: (* -> *) -> * -> *) m a
    = ComposeT { getComposeT :: f (g m) a }
  deriving
    ( Functor
    , Foldable
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadIO
    )

instance (MFunctor f, MonadTrans f, MonadTrans g) => MonadTrans (ComposeT f g)
  where
    lift = ComposeT . hoist lift . lift

instance Traversable (f (g m)) => Traversable (ComposeT f g m) where
    traverse f (ComposeT m) = fmap ComposeT (traverse f m)
