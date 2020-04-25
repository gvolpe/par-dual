# par-dual

[![CI Status](https://github.com/gvolpe/par-dual/workflows/Haskell%20CI/badge.svg)](https://github.com/gvolpe/par-dual/actions)

The [PureScript](https://www.purescript.org/) language defines a [Parallel](https://pursuit.purescript.org/packages/purescript-parallel/4.0.0/docs/Control.Parallel.Class#t:Parallel) typeclass in the `parallel` package. Quoting its documentation:

> The `Parallel` class abstracts over monads which support parallel composition via some related `Applicative`.

The same typeclass is defined in the [Scala](https://www.scala-lang.org/) language, as part of the [Cats](https://typelevel.org/cats/typeclasses/parallel.html) library.

This typeclass has been controversial, in a sense, for not having strong laws. However, it has been proven to be actually useful in real-world applications.

The idea of this package is to bring this power over to the Haskell language while exploring the design space to identify and define stronger laws (if possible).

Originally, this idea has been described in [this blogpost](https://gvolpe.github.io/blog/parallel-typeclass-for-haskell/).

## Dual

Here's the definition of the same typeclass in Haskell:

```haskell
class (Monad m, Applicative f) => Dual f m | m -> f, f -> m where
  parallel :: m :~> f
  sequential :: f :~> m
```

I decided to call it `Dual` instead of `Parallel`, because this *duality* doesn't always define a `sequential` and `parallel` relationship. Such is the case between `[]` and `ZipList`, as we will soon discover.

It defines two functions, which are natural transformations between a `Monad m` and an `Applicative f`. It could also be seen as a typeclass version of an isomorphism such as `forall a . Iso (f a) (m a)`.

The most common and useful relationships are both `Either` / `Validation` and `IO` / `Concurrently`.

```haskell
instance Semigroup e => Dual (Validation e) (Either e) where
  parallel   = NT fromEither
  sequential = NT toEither

instance Dual Concurrently IO where
  parallel   = NT Concurrently
  sequential = NT runConcurrently
```

`Validation` comes from the [validators](https://hackage.haskell.org/package/validators) package, whereas `Concurrently` comes from the [async](https://hackage.haskell.org/package/async) package.

Its power comes from the extra functions that have a default implementation, as we can see next.

### parMapN

The `parMapN` function is analogue to combining `<$>` and `<*>`, for any `Applicative` that is not a `Monad`.

```haskell
parMapN
  :: (Applicative f, Monad m, Dual f m)
  => m a0
  -> m a1
  -> (a0 -> a1 -> a)
  -> m a
```

In this case, it takes only two `Monad` computations and a function, but in practice, this function abstracts over its arity in order to allow us to compose an arbitrary number of computations.

For example, if we define a `Person` datatype with two fields:

```haskell
type Name = Refined NonEmpty String
type Age = Refined (GreaterThan 17) Int

data Person = Person
  { personAge :: Age
  , personName :: Name
  } deriving Show
```

We can then validate different inputs, while accumulating errors on the left side, even when our type is `Either [String] Person`.

```haskell
mkPerson :: Int -> String -> Either [String] Person
mkPerson a n = parMapN (ref a) (ref n) Person
```

Where `ref` is a generic function that converts `RefineException`s to `[String]`:

```haskell
ref :: Predicate p x => x -> Either [String] (Refined p x)
ref x = left (\e -> [show e]) (refine x)
```

In case of two invalid inputs, we will get as a result a list of validation errors:

```haskell
mkPerson 10 "" == Left ["error 1", "error 2"]
```

If `parMapN` didn't exist, we could do the same by manually converting between `Either` and `Validation` (which is exactly what `parMapN` does via the `Dual` class).

```haskell
mkPerson :: Int -> String -> Either [String] Person
mkPerson a n = toEither $ Person <$> fromEither (ref a) <*> fromEither (ref n)
```

Though, we can see how cumbersome and boilerplatey it gets.

### parTraverse

Another great application of the `Dual` class is the definition of a `traverse` function that takes a `Monad` and a `Traversable t`, but that operates over its dual `Applicative`, and at the end it converts back to this `Monad`.

```haskell
parTraverse
  :: (Traversable t, Applicative f, Monad m, Dual f m)
  => (a -> m b)
  -> t a
  -> m (t b)
```

The type signature is exactly the same as `traverse`, except the constraints are different.

We can appreciate its usability by looking at some examples. Here's one with `Either`:

```haskell
f :: Int -> Either [String] Int
f n = Left [show n]

traverse f [1..5] == Left ["1"]
parTraverse f [1..5] == Left ["1","2","3","4","5"]
```

Below is another one with `IO`:

```haskell
randomDelay :: IO ()
randomDelay = do
  r <- randomRIO (1, 10)
  threadDelay (r * 500000)

traverseIO :: IO ()
traverseIO = traverse_ (\n -> randomDelay >> print n) [1 .. 10]

parTraverseIO :: IO ()
parTraverseIO = parTraverse_ (\n -> randomDelay >> print n) [1 .. 10]
```

The `traverse` version prints out numbers from 1 to 10 in sequence, while waiting for every random delay. So the output is pretty much `1 2 3 4 5 6 7 8 9 10`.

The `parTraverse` version has a non-deterministic output, since it goes through `Concurrently` (`IO`'s dual). It is exactly what you would expect while using [mapConcurrently](). One possible output is `5 10 6 1 3 2 9 4 7 8`.

### ZipList

The dual `Applicative` instance of `[]` is the one defined by `ZipList`, which doesn't have anything to do with parallelism.

Let's have a look at the examples shown below.

```haskell
((+) <$> [1..5] <*> [6..10]) == [7,8,9,10,11,8,9,10,11,12,9,10,11,12,13,10,11,12,13,14,11,12,13,14,15]

parMapN [1..5] [6..10] (+) == [7,9,11,13,15]
```

The standard version iterates over both lists "sequentially". That is, it takes the first element of the first list, `1` in this case, and then iterates over the second list and it returns the sum of `1` and every value of the second list. It repeats the process until there are no elements left in the first list.

Conversely, `ZipList`s only return the sum of the current elements of both lists such as `1 + 6`, `2 + 7`, and so on. It iterates over both lists in "parallel", effectively only once.
