{-# LANGUAGE FunctionalDependencies, TypeOperators #-}

module Control.Dual.Class where

import           Control.Applicative            ( ZipList(..) )
import           Control.Concurrent.Async       ( Concurrently(..) )
import           Control.Natural                ( (:~>)(..)
                                                , (#)
                                                )
import           Data.Functor                   ( void )
import           Data.Validation                ( Validation
                                                , toEither
                                                , fromEither
                                                )

{-
 - The @Dual@ class abstracts over @Monad@s that have dual
 - @Applicative@ instance that acts in a different useful way.
 -
 - It can be seen as an isomorphism at the class level.
 -}
class (Monad m, Applicative f) => Dual f m | m -> f, f -> m where
  parallel :: m :~> f
  sequential :: f :~> m

  {-
  TODO: Abstract over its arity

  It is the analogue to using @<$>@ and @<*>@ for the dual
  @Applicative@ of the current @Monad@, as defined by the
  relationship defined by the @Dual@ instance.
  -}
  parMapN :: m a0 -> m a1 -> (a0 -> a1 -> a) -> m a
  parMapN ma0 ma1 f =
    (#) sequential (f <$> (#) parallel ma0 <*> (#) parallel ma1)

  {-
  A convenience function, defined in terms of @parMapN@
  -}
  parTupled :: m a0 -> m a1 -> m (a0, a1)
  parTupled ma0 ma1 = parMapN ma0 ma1 (,)

  {-
  Same as @traverse@, except it uses the dual @Applicative@ of
  the current @Monad@, as defined by the @Dual@ relationship.
  -}
  parTraverse :: Traversable t => (a -> m b) -> t a -> m (t b)
  parTraverse f xs =
    let g a = (#) parallel (f a)
        res = sequenceA $ fmap g xs
    in  (#) sequential res

  {-
  Same as @traverse_@, except it uses the dual @Applicative@ of
  the current @Monad@, as defined by the @Dual@ relationship.
  -}
  parTraverse_ :: Traversable t => (a -> m b) -> t a -> m ()
  parTraverse_ f = void . parTraverse f

  {-
  Same as @sequence@, except it uses the dual @Applicative@ of
  the current @Monad@, as defined by the @Dual@ relationship.
  -}
  parSequence :: Traversable t => t (m a) -> m (t a)
  parSequence xs = parTraverse id xs

  {-
  Same as @sequence_@, except it uses the dual @Applicative@ of
  the current @Monad@, as defined by the @Dual@ relationship.
  -}
  parSequence_ :: Traversable t => t (m a) -> m ()
  parSequence_ xs = parTraverse_ id xs

--------------------- Instances ----------------------------

instance Semigroup e => Dual (Validation e) (Either e) where
  parallel   = NT fromEither
  sequential = NT toEither

instance Dual Concurrently IO where
  parallel   = NT Concurrently
  sequential = NT runConcurrently

instance Dual ZipList [] where
  parallel   = NT ZipList
  sequential = NT getZipList
