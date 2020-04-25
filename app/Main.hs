{-# LANGUAGE DataKinds, OverloadedStrings #-}

module Main where

import           Control.Arrow                  ( left )
import           Control.Concurrent             ( threadDelay )
import           Control.Dual.Class
import           Data.Foldable                  ( traverse_ )
import           Data.Validation                ( fromEither
                                                , toEither
                                                )
import           Refined
import           System.Random                  ( randomRIO )

main :: IO ()
main = parTraverseIO
--main = print $ makePerson 10 ""

-------------- Datatypes -------------------------

type Name = Refined NonEmpty String
type Age = Refined (GreaterThan 17) Int

data Person = Person
  { personAge :: Age
  , personName :: Name
  } deriving Show

-------------- Sequential Validation -------------

mkPersonSeq :: Int -> String -> Either RefineException Person
mkPersonSeq a n = do
  age  <- refine a
  name <- refine n
  return $ Person age name

-------------- Parallel Validation (manually) -------------------

type Eff a = Either [String] a

ref :: Predicate p x => x -> Eff (Refined p x)
ref x = left (\e -> [show e]) (refine x)

mkPerson :: Int -> String -> Eff Person
mkPerson a n = toEither $ Person <$> fromEither (ref a) <*> fromEither (ref n)

-------------- Parallel Validation -------------

makePerson :: Int -> String -> Eff Person
makePerson a n = parMap2 (ref a) (ref n) Person

-------------- Par Traverse -----------------------

randomDelay :: IO ()
randomDelay = do
  r <- randomRIO (1, 10)
  threadDelay (r * 500000)

traverseEither :: IO ()
traverseEither =
  print (traverse (\n -> Left [show n]) [1 .. 10] :: Either [String] [Int])

parTraverseEither :: IO ()
parTraverseEither =
  print (parTraverse (\n -> Left [show n]) [1 .. 10] :: Either [String] [Int])

traverseIO :: IO ()
traverseIO = traverse_ (\n -> randomDelay >> print n) [1 .. 10]

parTraverseIO :: IO ()
parTraverseIO = parTraverse_ (\n -> randomDelay >> print n) [1 .. 10]

-------------- Zip Lists --------------------------

n1 :: [Int]
n1 = [1 .. 5]

n2 :: [Int]
n2 = [6 .. 10]

n3 :: [Int]
n3 = (+) <$> n1 <*> n2

n4 :: [Int]
n4 = parMap2 n1 n2 (+)
--n4 = getZipList $ (+) <$> ZipList n1 <*> ZipList n2
