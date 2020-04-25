module Control.Dual where

import           Control.Natural                ( unwrapNT )
import           Control.Dual.Class
import           Data.Functor                   ( void )

{-
 - TODO: Abstract over its arity
 -
 - It is the analogue to using @<$>@ and @<*>@ for the dual
 - @Applicative@ of the current @Monad@, as defined by the
 - relationship defined by the @Dual@ instance.
 -}
parMapN
  :: (Applicative f, Monad m, Dual f m)
  => m a0
  -> m a1
  -> (a0 -> a1 -> a)
  -> m a
parMapN ma0 ma1 f =
  unwrapNT sequential (f <$> unwrapNT parallel ma0 <*> unwrapNT parallel ma1)

{-
 - A convenience function, defined in terms of @parMapN@
 -}
parTupled
  :: (Applicative f, Monad m, Dual f m) => m a0 -> m a1 -> m (a0, a1)
parTupled ma0 ma1 = parMapN ma0 ma1 (,)

{-
 - Same as @traverse@, except it uses the dual @Applicative@ of
 - the current @Monad@, as defined by the @Dual@ relationship.
 -}
parTraverse
  :: (Traversable t, Applicative f, Monad m, Dual f m)
  => (a -> m b)
  -> t a
  -> m (t b)
parTraverse f xs =
  let g a = unwrapNT parallel (f a)
      res = sequenceA $ fmap g xs
  in  unwrapNT sequential res

{-
 - Same as @traverse_@, except it uses the dual @Applicative@ of
 - the current @Monad@, as defined by the @Dual@ relationship.
 -}
parTraverse_
  :: (Traversable t, Applicative f, Monad m, Dual f m)
  => (a -> m b)
  -> t a
  -> m ()
parTraverse_ f = void . parTraverse f

{-
 - Same as @sequence@, except it uses the dual @Applicative@ of
 - the current @Monad@, as defined by the @Dual@ relationship.
 -}
parSequence
  :: (Traversable t, Applicative f, Monad m, Dual f m)
  => t (m a)
  -> m (t a)
parSequence xs = parTraverse id xs

{-
 - Same as @sequence_@, except it uses the dual @Applicative@ of
 - the current @Monad@, as defined by the @Dual@ relationship.
 -}
parSequence_
  :: (Traversable t, Applicative f, Monad m, Dual f m)
  => t (m a)
  -> m ()
parSequence_ xs = parTraverse_ id xs
