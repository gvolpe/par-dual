{-# LANGUAGE DataKinds, OverloadedStrings, TemplateHaskell #-}

module Main where

import           Control.Arrow                  ( left )
import           Control.Concurrent             ( threadDelay )
import           Control.Dual.Class
import           Control.Monad                  ( unless )
import           Data.Bitraversable             ( bitraverse )
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.Validation                ( fromEither
                                                , toEither
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Refined
import           System.Exit                    ( exitFailure )

main :: IO ()
main = do
  results <- sequence [checkParallel dualTests]
  unless (and results) exitFailure

dualTests :: Group
dualTests = $$(discover)

prop_parMap2_on_success :: Property
prop_parMap2_on_success = property $ do
  a <- forAll $ Gen.int (Range.linear 18 100)
  n <- forAll $ Gen.list (Range.linear 1 50) Gen.alpha
  let result   = parMap2 (ref a) (ref n) Person
      expected = Person <$> ref a <*> ref n
  result === expected

prop_parMap2_accumulates_errors :: Property
prop_parMap2_accumulates_errors = property $ do
  a <- forAll $ Gen.int (Range.linear 0 17)
  n <- forAll $ Gen.list (Range.linear 0 0) Gen.alpha
  let
    res1 = parMap2 (ref a) (ref n) Person
    res2 = Person <$> ref a <*> ref n
    exp1 = Left
      [ "The predicate (GreaterThan 17) does not hold: \n  Value is not greater than 17"
      , "The predicate (SizeGreaterThan 0) does not hold: \n  Size of Foldable is not greater than 0\n  Size is: 0"
      ]
    exp2 = left (take 1) exp1
  res1 === exp1
  res2 === exp2

prop_parTraverse_accumulates_errors :: Property
prop_parTraverse_accumulates_errors = property $ do
  xs <- forAll $ Gen.list (Range.linear 1 10) (Gen.int (Range.linear 1 10))
  let f :: Int -> Either [String] Int
      f n = Left [show n]
      res1 = parTraverse f xs
      res2 = traverse f xs
      exp1 = Left (show <$> xs)
      exp2 = Left (take 1 $ show <$> xs)
  res1 === exp1
  res2 === exp2

-- This one is tricky to test but this seems good enough for now
prop_parTraverse_io_is_concurrent :: Property
prop_parTraverse_io_is_concurrent = withTests (10 :: TestLimit) $ property $ do
  xs <- forAll $ Gen.list (Range.linear 15 25) (Gen.int (Range.linear 1 10))
  let f r n = threadDelay (1 * 3000) >> atomicModifyIORef r (\x -> (n : x, n))
  ref1 <- evalIO $ newIORef [] :: PropertyT IO (IORef [Int])
  ref2 <- evalIO $ newIORef [] :: PropertyT IO (IORef [Int])
  evalIO $ parTraverse_ (f ref1) xs
  evalIO $ traverse_ (f ref2) xs
  res1 <- evalIO $ readIORef ref1
  res2 <- evalIO $ readIORef ref2
  -- Avoid cases where all elements are the same (could prob. be done in a better way)
  let exp1 = if and (fmap (== head xs) (tail xs)) then [] else reverse xs
  res1 /== exp1
  res2 === reverse xs

prop_parMap2_on_lists :: Property
prop_parMap2_on_lists = property $ do
  xs <- forAll $ Gen.constant [1 .. 5] :: PropertyT IO [Int]
  ys <- forAll $ Gen.constant [6 .. 10] :: PropertyT IO [Int]
  let res1 = parMap2 xs ys (+)
      res2 = (+) <$> xs <*> ys
      exp1 = [7, 9, 11, 13, 15]
      exp2 = [7 .. 11] ++ [8 .. 12] ++ [9 .. 13] ++ [10 .. 14] ++ [11 .. 15]
  res1 === exp1
  res2 === exp2

prop_parBitraverse :: Property
prop_parBitraverse = property $ do
  a <- forAll $ Gen.list (Range.linear 5 10) Gen.alpha
  b <- forAll $ Gen.int (Range.linear 5 10)
  c <- forAll Gen.bool
  let res1 = parBitraverse show show (a, b, c)
      res2 = bitraverse show show (a, b, c)
  length res1 === min (length $ show b) (length $ show c)
  length res2 === length (show b) * length (show c)

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
