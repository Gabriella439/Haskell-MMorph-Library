{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE Trustworthy                #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif

{-| Composition of monad transformers. A higher-order version of
    "Data.Functor.Compose".
-}

module Control.Monad.Trans.Compose (
    -- * ComposeT
    ComposeT(ComposeT, getComposeT),
    mapComposeT
   ) where

import Control.Applicative (
    Applicative(pure, (<*>), (*>), (<*)), Alternative(empty, (<|>)) )
import Control.Monad (MonadPlus(mzero, mplus), liftM)
import Control.Monad.Cont.Class (MonadCont(callCC))
import Control.Monad.Error.Class (MonadError(throwError, catchError))
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader(ask, local, reader))
import Control.Monad.State.Class (MonadState(get, put, state))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Writer.Class (MonadWriter(writer, tell, listen, pass))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (Foldable(fold, foldMap, foldr, foldl, foldr1, foldl1))
import Data.Traversable (Traversable(traverse, sequenceA, mapM, sequence))
import Prelude hiding (foldr, foldl, foldr1, foldl1, mapM, sequence)

infixr 9 `ComposeT`

-- | Composition of monad transformers.
newtype ComposeT (f :: (* -> *) -> * -> *) (g :: (* -> *) -> * -> *) m a
    = ComposeT { getComposeT :: f (g m) a }
    deriving
    ( Alternative
    , Applicative
    , Eq
    , Foldable
    , Functor
    , Ord
    , Read
    , Show
    , Traversable
    , Monad
    , MonadCont
    , MonadError e
    , MonadFail
    , MonadIO
    , MonadPlus
    , MonadReader r
    , MonadRWS r w s
    , MonadState s
    , MonadWriter w
    )

instance (MFunctor f, MonadTrans f, MonadTrans g) => MonadTrans (ComposeT f g)
  where
    lift = ComposeT . hoist lift . lift

#if __GLASGOW_HASKELL__ >= 806
instance (MFunctor f, MFunctor g, forall m. Monad m => Monad (g m))
    => MFunctor (ComposeT f g) where
    hoist f (ComposeT m) = ComposeT (hoist (hoist f) m)
#endif

-- | Transform the computation inside a 'ComposeT'.
mapComposeT :: (f (g m) a -> p (q n) b) -> ComposeT f g m a -> ComposeT p q n b
mapComposeT f = ComposeT . f . getComposeT
