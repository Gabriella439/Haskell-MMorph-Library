{-| A monad morphism is a natural transformation:

> morph :: forall a . m a -> n a

    ... that obeys the following two laws:

> morph $ do x <- m  =  do x <- morph m
>            f x           morph (f x)
>
> morph (return x) = return x

    ... which are equivalent to the following two functor laws:

> morph . (f >=> g) = morph . f >=> morph . g
>
> morph . return = return

    Examples of monad morphisms include:

    * 'lift' (from 'MonadTrans')

    * 'squash' (See below)

    * @'hoist' f@ (See below), if @f@ is a monad morphism

    * @(f . g)@, if @f@ and @g@ are both monad morphisms

    * 'id'

    Monad morphisms commonly arise when manipulating monad transformers for
    compatibility with other monad transformers.  The 'MFunctor', 'MonadTrans',
    and 'MMonad' classes define standard ways to change the shape of monad
    transformer stacks:

    * 'lift' introduces a new monad transformer layer of any type.

    * 'squash' flattens two identical monad transformer layers into a single
      layer of the same type.

    * 'hoist' maps monad morphisms to modify deeper layers of the monad
       transformer stack.

-}

{-# LANGUAGE Rank2Types #-}

module Control.Monad.Morph (
    -- * Functors over Monads
    MFunctor(..),
    -- * Monads over Monads
    MMonad(..),
    MonadTrans(lift),
    squash,
    (>|>),
    (<|<),
    (=<|),
    (|>=)
    ) where

import Control.Monad.Trans.Class (MonadTrans(lift))
import qualified Control.Monad.Trans.Error         as E
import qualified Control.Monad.Trans.Identity      as I
import qualified Control.Monad.Trans.Maybe         as M
import qualified Control.Monad.Trans.Reader        as R
import qualified Control.Monad.Trans.RWS.Lazy      as RWS
import qualified Control.Monad.Trans.RWS.Strict    as RWS'
import qualified Control.Monad.Trans.State.Lazy    as S 
import qualified Control.Monad.Trans.State.Strict  as S'
import qualified Control.Monad.Trans.Writer.Lazy   as W'
import qualified Control.Monad.Trans.Writer.Strict as W
import Data.Monoid (Monoid, mappend)

-- For documentation
import Control.Monad ((=<<), (>=>), (<=<), join)

{-| A functor in the category of monads, using 'hoist' as the analog of 'fmap':

> hoist (f . g) = hoist f . hoist g
>
> hoist id = id
-}
class MFunctor t where
    {-| Lift a monad morphism from @m@ to @n@ into a monad morphism from
        @(t m)@ to @(t n)@
    -}
    hoist :: (Monad m) => (forall a . m a -> n a) -> t m b -> t n b

instance MFunctor (E.ErrorT e) where
    hoist nat m = E.ErrorT (nat (E.runErrorT m))

instance MFunctor I.IdentityT where
    hoist nat m = I.IdentityT (nat (I.runIdentityT m))

instance MFunctor M.MaybeT where
    hoist nat m = M.MaybeT (nat (M.runMaybeT m))

instance MFunctor (R.ReaderT r) where
    hoist nat m = R.ReaderT (\i -> nat (R.runReaderT m i))

instance MFunctor (RWS.RWST r w s) where
    hoist nat m = RWS.RWST (\r s -> nat (RWS.runRWST m r s))

instance MFunctor (RWS'.RWST r w s) where
    hoist nat m = RWS'.RWST (\r s -> nat (RWS'.runRWST m r s))

instance MFunctor (S.StateT s) where
    hoist nat m = S.StateT (\s -> nat (S.runStateT m s))

instance MFunctor (S'.StateT s) where
    hoist nat m = S'.StateT (\s -> nat (S'.runStateT m s))

instance MFunctor (W.WriterT w) where
    hoist nat m = W.WriterT (nat (W.runWriterT m))

instance MFunctor (W'.WriterT w) where
    hoist nat m = W'.WriterT (nat (W'.runWriterT m))

{-| A monad in the category of monads, using 'lift' from 'MonadTrans' as the
    analog of 'return' and 'embed' as the analog of ('=<<'):

> embed lift = id
>
> embed f (lift m) = f m
>
> embed g (embed f t) = embed (\m -> embed g (f m)) t
-}
class (MFunctor t, MonadTrans t) => MMonad t where
    {-| Embed a newly created 'MMonad' layer within an existing layer

        'embed' is analogous to ('=<<')
    -}
    embed :: (Monad n) => (forall a . m a -> t n a) -> t m b -> t n b

{-| Squash two 'MMonad' layers into a single layer

    'squash' is analogous to 'join'
-}
squash :: (Monad m, MMonad t) => t (t m) a -> t m a
squash = embed id

infixr 2 >|>, =<|
infixl 2 <|<, |>=

{-| Compose two 'MMonad' layer-building functions

    ('>|>') is analogous to ('>=>')
-}
(>|>)
    :: (Monad m3, MMonad t)
    => (forall a . m1 a -> t m2 a)
    -> (forall b . m2 b -> t m3 b)
    ->             m1 c -> t m3 c
(f >|> g) m = embed g (f m)

{-| Equivalent to ('>|>') with the arguments flipped

    ('<|<') is analogous to ('<=<')
-}
(<|<)
    :: (Monad m3, MMonad t)
    => (forall b . m2 b -> t m3 b)
    -> (forall a . m1 a -> t m2 a)
    ->             m1 c -> t m3 c
(g <|< f) m = embed g (f m)

{-| An infix operator equivalent to 'embed'

    ('=<|') is analogous to ('=<<')
-}
(=<|) :: (Monad n, MMonad t) => (forall a . m a -> t n a) -> t m b -> t n b
(=<|) = embed

{-| Equivalent to ('=<|') with the arguments flipped

    ('|>=') is analogous to ('>>=')
-}
(|>=) :: (Monad n, MMonad t) => t m b -> (forall a . m a -> t n a) -> t n b
t |>= f = embed f t

instance (E.Error e) => MMonad (E.ErrorT e) where
    embed f m = E.ErrorT (do 
        x <- E.runErrorT (f (E.runErrorT m))
        return (case x of
            Left         e  -> Left e
            Right (Left  e) -> Left e
            Right (Right a) -> Right a ) )

instance MMonad I.IdentityT where
    embed f m = f (I.runIdentityT m)

instance MMonad M.MaybeT where
    embed f m = M.MaybeT (do
        x <- M.runMaybeT (f (M.runMaybeT m))
        return (case x of
            Nothing       -> Nothing
            Just Nothing  -> Nothing
            Just (Just a) -> Just a ) )

instance MMonad (R.ReaderT r) where
    embed f m = R.ReaderT (\i -> R.runReaderT (f (R.runReaderT m i)) i)

instance (Monoid w) => MMonad (W.WriterT w) where
    embed f m = W.WriterT (do
        ~((a, w1), w2) <- W.runWriterT (f (W.runWriterT m))
        return (a, mappend w1 w2) )

instance (Monoid w) => MMonad (W'.WriterT w) where
    embed f m = W'.WriterT (do
        ((a, w1), w2) <- W'.runWriterT (f (W'.runWriterT m))
        return (a, mappend w1 w2) )
