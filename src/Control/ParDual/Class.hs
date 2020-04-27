{-# LANGUAGE FunctionalDependencies, TypeOperators #-}

{-|
Module      : Control.ParDual.Class
Description : Definition of the 'ParDual' class and its functions.
Copyright   : (c) Gabriel Volpe, 2020
License     : Apache-2.0
Maintainer  : volpegabriel@gmail.com
Stability   : experimental

You can find here functions such as 'parMap2', 'parTraverse', 'parBitraverse', etc.
-}
module Control.ParDual.Class where

import           Control.Applicative            ( ZipList(..) )
import           Control.Concurrent.Async       ( Concurrently(..) )
import           Control.Natural                ( (:~>)(..)
                                                , (#)
                                                )
import           Data.Bitraversable             ( Bitraversable
                                                , bitraverse
                                                )
import           Data.Functor                   ( void )
import           Data.Validation                ( Validation
                                                , toEither
                                                , fromEither
                                                )

{- | The ParDual class abstracts over 'Monad's that have a dual
'Applicative' instance that acts in a different useful way.

E.g., the duality between 'Either' and 'Validation'. As well
as the duality between 'IO' and 'Concurrently'.

It can also be seen as an isomorphism defined at the class level.
-}
class (Monad m, Applicative f) => ParDual f m | m -> f, f -> m where
  {- | A natural transformation from 'm' to 'f'
  -}
  parallel :: m :~> f

  {- | A natural transformation from 'f' to 'm'
  -}
  sequential :: f :~> m

  {- |
  It is the analogue to using '<$>' and '<*>' for the dual
  'Applicative' of the current 'Monad', as defined by the
  relationship defined by the 'ParDual' instance.
  -}
  parMap2 :: m a0 -> m a1 -> (a0 -> a1 -> a) -> m a
  parMap2 ma0 ma1 f = (#) sequential $ f
    <$> (#) parallel ma0
    <*> (#) parallel ma1

  {- |
  It is the analogue to using '<$>' and '<*>' for the dual
  'Applicative' of the current 'Monad', as defined by the
  relationship defined by the 'ParDual' instance.
  -}
  parMap3 :: m a0 -> m a1 -> m a2 -> (a0 -> a1 -> a2 -> a) -> m a
  parMap3 ma0 ma1 ma2 f = (#) sequential $ f
    <$> (#) parallel ma0
    <*> (#) parallel ma1
    <*> (#) parallel ma2

  {- |
  It is the analogue to using '<$>' and '<*>' for the dual
  'Applicative' of the current 'Monad', as defined by the
  relationship defined by the 'ParDual' instance.
  -}
  parMap4 :: m a0 -> m a1 -> m a2 -> m a3 -> (a0 -> a1 -> a2 -> a3 -> a) -> m a
  parMap4 ma0 ma1 ma2 ma3 f = (#) sequential $ f
    <$> (#) parallel ma0
    <*> (#) parallel ma1
    <*> (#) parallel ma2
    <*> (#) parallel ma3

  {- |
  It is the analogue to using '<$>' and '<*>' for the dual
  'Applicative' of the current 'Monad', as defined by the
  relationship defined by the 'ParDual' instance.
  -}
  parMap5 :: m a0 -> m a1 -> m a2 -> m a3 -> m a4 -> (a0 -> a1 -> a2 -> a3 -> a4 -> a) -> m a
  parMap5 ma0 ma1 ma2 ma3 ma4 f = (#) sequential $ f
    <$> (#) parallel ma0
    <*> (#) parallel ma1
    <*> (#) parallel ma2
    <*> (#) parallel ma3
    <*> (#) parallel ma4

  {- |
  It is the analogue to using '<$>' and '<*>' for the dual
  'Applicative' of the current 'Monad', as defined by the
  relationship defined by the 'ParDual' instance.
  -}
  parMap6 :: m a0 -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a) -> m a
  parMap6 ma0 ma1 ma2 ma3 ma4 ma5 f = (#) sequential $ f
    <$> (#) parallel ma0
    <*> (#) parallel ma1
    <*> (#) parallel ma2
    <*> (#) parallel ma3
    <*> (#) parallel ma4
    <*> (#) parallel ma5

  {- |
  Same as 'traverse', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parTraverse :: Traversable t => (a -> m b) -> t a -> m (t b)
  parTraverse f ta =
    let g a = (#) parallel (f a)
        res = sequenceA $ fmap g ta
    in  (#) sequential res

  {- |
  Same as 'Data.Foldable.traverse_', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parTraverse_ :: Traversable t => (a -> m b) -> t a -> m ()
  parTraverse_ f = void . parTraverse f

  {- |
  Same as 'sequence', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parSequence :: Traversable t => t (m a) -> m (t a)
  parSequence = parTraverse id

  {- |
  Same as 'sequence_', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parSequence_ :: Traversable t => t (m a) -> m ()
  parSequence_ = void . parSequence

  {- |
  Same as '*>', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parProductR :: m a -> m b -> m b
  parProductR ma mb = parMap2 ma mb (\_ b -> b)

  {- |
  Same as '<*', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parProductL :: m a -> m b -> m a
  parProductL ma mb = parMap2 ma mb const

  {- |
  Same as 'bitraverse', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parBitraverse :: Bitraversable t => (a -> m c) -> (b -> m d) -> t a b -> m (t c d)
  parBitraverse ma mb tab =
    let fa  = (\a -> (#) parallel (ma a))
        fb  = (\b -> (#) parallel (mb b))
        res = bitraverse fa fb tab
    in  (#) sequential res

  {- |
  Same as 'Data.Traversable.bisequence', except it uses the dual 'Applicative' of
  the current 'Monad', as defined by the 'ParDual' relationship.
  -}
  parBisequence :: Bitraversable t => t (m a) (m b) -> m (t a b)
  parBisequence = parBitraverse id id

--------------------- Instances ----------------------------

instance Semigroup e => ParDual (Validation e) (Either e) where
  parallel   = NT fromEither
  sequential = NT toEither

instance ParDual Concurrently IO where
  parallel   = NT Concurrently
  sequential = NT runConcurrently

instance ParDual ZipList [] where
  parallel   = NT ZipList
  sequential = NT getZipList
