{-# LANGUAGE FunctionalDependencies, TypeOperators #-}

module Control.Dual.Class where

import           Control.Applicative            ( ZipList(..) )
import           Control.Concurrent.Async       ( Concurrently(..))
import           Control.Natural                ( (:~>)(..) )
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

instance Semigroup e => Dual (Validation e) (Either e) where
  parallel   = NT fromEither
  sequential = NT toEither

instance Dual ZipList [] where
  parallel   = NT ZipList
  sequential = NT getZipList

instance Dual Concurrently IO where
  parallel   = NT Concurrently
  sequential = NT runConcurrently
