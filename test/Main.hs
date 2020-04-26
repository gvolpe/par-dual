{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell #-}

module Main where

import           Control.Arrow                  ( left )
import           Control.Concurrent             ( threadDelay )
import           Control.Dual.Class
import           Control.Monad                  ( unless )
import           Data.Bitraversable             ( bitraverse )
import           Data.Foldable                  ( traverse_ )
import           Data.Validation                ( fromEither
                                                , toEither
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Refined
import           System.Exit (exitFailure)

main :: IO ()
main = do
  results <- sequence [checkParallel dualTests]
  unless (and results) $ exitFailure

prop_parMap2_eq_either_on_success :: Property
prop_parMap2_eq_either_on_success = property $ do
  a <- forAll $ Gen.int (Range.linear 18 100)
  n <- forAll $ Gen.list (Range.linear 1 50) Gen.alpha
  let res = parMap2 (ref a) (ref n) Person
      exp = Person <$> ref a <*> ref n
  res === exp

prop_parMap2_accumulates_errors :: Property
prop_parMap2_accumulates_errors = property $ do
  a <- forAll $ Gen.int (Range.linear 0 17)
  n <- forAll $ Gen.list (Range.linear 0 0) Gen.alpha
  let
    res = parMap2 (ref a) (ref n) Person
    exp = Left
      [ "The predicate (GreaterThan 17) does not hold: \n  Value is not greater than 17"
      , "The predicate (SizeGreaterThan 0) does not hold: \n  Size of Foldable is not greater than 0\n  Size is: 0"
      ]
  res === exp

dualTests :: Group
dualTests = $$(discover)

-------------- Datatypes -------------------------

type Name = Refined NonEmpty String
type Age = Refined (GreaterThan 17) Int

data Person = Person
  { personAge :: Age
  , personName :: Name
  } deriving (Eq, Show)

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

fixedDelay :: IO ()
fixedDelay = threadDelay (1 * 500000)

traverseEither :: IO ()
traverseEither =
  print (traverse (\n -> Left [show n]) [1 .. 10] :: Either [String] [Int])

parTraverseEither :: IO ()
parTraverseEither =
  print (parTraverse (\n -> Left [show n]) [1 .. 10] :: Either [String] [Int])

traverseIO :: IO ()
traverseIO = traverse_ (\n -> fixedDelay >> print n) [1 .. 10]

parTraverseIO :: IO ()
parTraverseIO = parTraverse_ (\n -> fixedDelay >> print n) [1 .. 10]

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

-------------- Par BiTraverse -----------------------

bitraverseTuple3 :: IO ()
bitraverseTuple3 =
  print (bitraverse (\n -> id (show n)) (\n -> id (show n)) ("ba", 24, True))

parBitraverseTuple3 :: IO ()
parBitraverseTuple3 =
  print (parBitraverse (\n -> id (show n)) (\n -> id (show n)) ("ba", 24, True))
