---
title: Bulk Operations with Traversals, Now Composed
---

* TOC
{:toc}

## Recap

[As I've written previously](2023-10-24-organizing-bulk-operations-with-traversals), if you have an operation
```hs
process :: A -> IO B
```
and you need to do this operation in bulk, i.e. turn multiple `A`'s into multiple `B`'s, then it's a good idea to represent it as
```hs
processBulk :: Traversable t => t A -> IO (t B)
```

This type describes an invariant, that the `B`'s returned are in a one-to-one correspondence with the input `A`'s, and forces us to eagerly check/enforce this invariant. As we'll see, this actually also makes such bulk operations *composable*.
<!-- more -->

## Sequencing

Throughout this post we'll use a running example of orchestrating bulk select queries to a database with the following toy schema:
```
┌─────┐               ┌──────┐               ┌────────┐
│Users│──[1-to-many]──│Orders│──[1-to-many]──│Products│
└─────┘               └──────┘               └────────┘
                         │              ┌────────┐
                         └─[1-to-many]──│Payments│
                                        └────────┘
```

If we have two bulk processes that we need to pipe one after another, then it's no surprise that we can just compose them with `>=>`:

```hs
orderIDByPaymentID
  :: Traversable t => t PaymentID -> IO (t OrderID)

userIDByOrderID
  :: Traversable t => t OrderID -> IO (t UserID)

userIDByPaymentID
  :: Traversable t => t PaymentID -> IO (t UserID)
userIDByPaymentID
  = orderIDByPaymentID >=> userIDByOrderID
```

## Nesting

Suppose for some reason we've decided to separate the function that selects product ID's that belong to an order, and the function that selects the actual data of the product by its ID.
```hs
productIDsByOrderID
  :: Traversable t => t OrderID -> IO (t [ProductID])
  -- ^ The nested [] is to reflect the multiplicity relationship:
  -- One order ID maps to 0 or more product ID's

productDataByID
  :: Traversable t => t ProductID -> IO (t ProductData)

productDatasByOrderID
  :: Traversable t => t OrderID -> IO (t [ProductData])
productDatasByOrderID = ???
```

How do we compose the two functions? We can start with `productIDsByOrderID >=> ???` but then the resulting `???` has expected type `t [ProductID] -> IO (t [ProductData])`, which is not what we've got.

Note that the choice of `t` we make when calling `productDataByID` doesn't have to match the `t` we're given as implementors of `productDatasByOrderID`. Instead what we can do is use `productDataByID` at type `Compose t []`:
```hs
productDataByID @(Compose t [])
  :: Compose t [] ProductID -> IO (Compose t [] ProductData)
```
With some newtype wrapping/unwrapping, we get:
```hs
fmap getCompose
  . productDataByID
  . Compose @t @[]
  :: t [ProductID] -> IO (t [ProductData])
```
i.e. exactly what we want. Putting the puzzle pieces together, we can implement the function:
```hs
productDatasByOrderID
  :: Traversable t => t OrderID -> IO (t [ProductData])
productDatasByOrderID orderIDs = do
  (productIDs :: t [ProductID]) <- productIDsByOrderID orderIDs
  (productDatas :: Compose t [] ProductData) <-
    productDataByID (Compose productIDs)
  pure $ getCompose productDatas
```
This possibility is due to there existing an `instance (Traversable f, Traversable g) => Traversable (Compose f g)`.


## Zipping

Suppose we want to select the data of an order by ID, and also the related products:
```hs
orderDataByID
  :: Traversable t => t OrderID -> IO (t OrderData)

productDatasByOrderID -- implemented up above
  :: Traversable t => t OrderID -> IO (t [ProductData])

orderWithProductsByID
  :: Traversable t => t OrderID -> IO (t (OrderData, [ProductData]))
orderWithProductsByID = ???
```
We could start with taking the collection of order IDs and feeding them to the two functions like so:
```hs
orderWithProductsByID orderIDs = do
  (orderDatas :: t OrderData) <- orderDataByID orderIDs
  (productDatas :: t [ProductData]) <- productDatasByOrderID orderIDs
  ???
```
but then we're left with a puzzle of how to zip together two `t` structures. However the only thing we know about `t` is that it is `Traversable`, and that is [not actually enough](2024-01-01-semialign-intuition) to zip arbitrary structures together.

We may note that they're not actually arbitrary structures, as each operation here preserves the shape, and thus `orderDatas` and `productDatas` must have the same shape and thus we're morally entitled to zipping them. You may be reaching for `mapAccumL` to unsafely zip them, or for a `Bazaar` to represent them as length-indexed vectors, but there's a much more safe and direct alternative to this.

We're going to get `productDatasByOrderID` to do the zipping for us, and we're going to do it by passing `OrderData` through it, and we're going to do it for free, *without changing `productDatasByOrderID`*.

The trick is to tuck `OrderData` into the `t`. Just like we represented `t [X]` as `Compose t [] X`, we can represent `t (X, Y)` as `Compose ((,) X) Y`:
```hs
fmap getCompose
  . productDatasByOrderID
  . Compose @t @((,) OrderData)
  :: t (OrderData, OrderID)
  -> IO (t (OrderData, [ProductData]))
```

Alright, but how do we get `t (OrderData, OrderID)` if what we have is `t OrderData` and `t OrderID`? We can do the exact same trick again: we thread (a copy of) `OrderID` through `orderDataByID`:
```hs
fmap getCompose
  . orderDataByID
  . Compose @t @((,) OrderID)
  :: t (OrderID, OrderID)
  -> IO (t (OrderID, OrderData))
```
With a little glue we can put the puzzle pieces together:
```hs
orderWithProductsByID orderIDs = do
  let
    dupedIDs :: t (OrderID, OrderID)
    dupedIDs = orderIDs <&> \i -> (i, i)
  Compose (idsAndOrderDatas :: t (OrderID, OrderData)) <-
    orderDataByID (Compose dupedIDs)
  let
    orderDatasAndIDs :: t (OrderData, OrderID)
    orderDatasAndIDs = idsAndOrderDatas <&> swap
  Compose (ordersAndProducts :: t (OrderData, [ProductData])) <-
    productDatasByOrderID (Compose orderDatasAndIDs)
  pure ordersAndProducts
```

## Scalability

Equipped with these tricks we fear no query:
```hs
usersEverything
  :: Traversable t
  => t UserID
  -> IO (t (UserData, [(OrderData, [ProductData], [PaymentData])]))
usersEverything userIDs = do
  let uiui = userIDs <&> \i -> (i, i)
  Compose uiud <- userDataByID (Compose uiui)
  let udui = uiud <&> swap
  Compose udoi <- orderIDsByUserID (Compose udui)
  let udoioi = Compose (Compose udoi) <&> \i -> (i, i)
  Compose udoiod <- orderDataByID (Compose udoioi)
  let udodoioi = Compose (udoiod <&> swap) <&> \i -> (i, i)
  Compose udodoipi <- productIDsByOrderID (Compose udodoioi)
  Compose (Compose udodoipd) <- productDataByID (Compose (Compose udodoipi))
  let udodpdoi = Compose (udodoipd <&> swap)
  udodpdyi <- paymentIDsByOrderID udodpdoi
  udodpdyd <- paymentDataByID (Compose udodpdyi)
  pure $ getCompose $ getCompose $ fmap (\(x, (y, z)) -> (x, y, z))
    $ getCompose $ getCompose $ getCompose udodpdyd
```
Actually maybe I fear that query. Can you keep track of the types, cause I for sure cannot. What's the type of `udodpdyd`?
```hs
Compose
  (Compose
    (Compose
      (Compose
        (Compose
          t
          ((,) UserData)
        )
        []
      )
      ((,) OrderData)
    )
    ((,) [ProductData])
  )
  []
  PaymentData
```
There's got to be a better way.

## Abstraction

Up to this point we've been treating the traversable approach as a "design pattern". Haskell's rich type system actually allows us to precisely capture the nature of this pattern and abstract it as a type:
```hs
newtype BulkFunction a b
  = BulkFunction (forall t. Traversable t => t a -> IO (t b))

productDataByID :: BulkFunction ProductID ProductData
```

Now we can also abstract the compositional glue we've used into well-defined functions. Sequencing gets the simple type:
```hs
BulkFunction x y -> BulkFunction y z -> BulkFunction x z
```
Actually [the base library has a typeclass](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Category.html) for this kind of operation, and having made a newtype, we can write an instance:
```hs
class Category (q :: Type -> Type -> Type) where
  id :: q x x
  (.) :: q y z -> q x y -> q x z

instance Category BulkFunction where
  id = BulkFunction pure
  BulkFunction f . BulkFunction g = BulkFunction (f <=< g)
```
Actually before we go too far, note that there is already [another instance of `Category`](https://hackage.haskell.org/package/base-4.19.1.0/docs/src/Control.Arrow.html#line-196), whose `id` is `pure` and whose `.` is `<=<`:
```hs
newtype Kleisli m a b = Kleisli (a -> m b)
instance Monad m => Category (Kleisli m)
```
It makes sense to abstract out the `_ -> IO _` part as a parameter, and delegate to its instance of `Category`:
```hs
newtype Bulk p a b = Bulk
  { runBulk :: forall t. Traversable t => p (t a) (t b) }

instance Category p => Category (Bulk p) where
  id = Bulk id
  Bulk f . Bulk g = Bulk (f . g)

productDataByID :: Bulk (Kleisli IO) ProductID ProductData
```

We're also going to need to be able to map over the `a` and `b` parameters, this is achieved by making it an instance of `Profunctor`, from the [profunctors](https://hackage.haskell.org/package/profunctors) library:
```hs
class Profunctor (p :: Type -> Type -> Type) where
  dimap :: (x -> y) -> (z -> w) -> p y z -> p x w

instance Profunctor p => Profunctor (Bulk p) where
  dimap f g (Bulk h) = Bulk (dimap (fmap f) (fmap g) h)
```

## Another Take at Nesting

Next up we have a notion of nesting: we were able to "lift" `productDataByID` like so:
```hs
productDataByID :: Bulk (Kleisli IO) ProductID ProductData

productDatasByIDs :: Bulk (Kleisli IO) [ProductID] [ProductData]
productDatasByIDs = Bulk
  (dimap Compose getCompose $ runBulk productDataByID)
```
Generalizing this, we have:
```hs
liftList :: Profunctor p => Bulk p x y -> Bulk p [x] [y]
```
In fact, generalizing even further, we arrive at:
```hs
liftContainer
  :: (Profunctor p, Traversable t)
  => Bulk p x y -> Bulk p (t x) (t y)
liftContainer (Bulk f) = Bulk (dimap Compose getCompose f)
```

The `profunctors` library has a typeclass for profunctors that support this operation:
```hs
class Traversing q where
  traverse' :: Traversable t => q x y -> q (t x) (t y)

instance Profunctor p => Traversing (Bulk p) where
  traverse' = liftContainer
```

## Another Take at Zipping

The trick of tucking/threading data into the traversable is an example of something called profunctor strength:
```hs
class Strong q where
  first' :: q x y -> q (x, z) (y, z)
  second' :: q x y -> q (z, x) (z, y)

instance Profunctor p => Strong (Bulk p) where
  second' = traverse'
  -- ^ slick definition also leveraging `Compose t ((,) c)`
  first' f = dimap swap swap $ second' f
```

With these two functions we can define a "fan-out" operation:
```hs
fanout :: (Category q, Strong q) => q x y -> q x z -> q x (y, z)
fanout f g = dimap (\x -> (x, x)) id
  (second' g . first' f)
```
which can in particular be specialized to `q ~ Bulk p`, whence it supplies the same input to two processes, and "zips" their results.

## Arrows

What we've just defined looks very similar to the `&&&` operator from [Control.Arrow](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Arrow.html#v:-38--38--38-), both in type signature and in implementation:
```hs
class Category q => Arrow q where
  arr :: (x -> y) -> q x y
  first :: q x y -> q (x, z) (y, z)
  second :: q x y -> q (z, x) (z, y)

  (***) :: q x y -> q z w -> q (x, z) (y, w)
  f *** g = second g . first f
  (&&&) :: q x y -> q x z -> q x (y, z)
  f &&& g = (f *** g) . arr (\x -> (x, x))
```
And indeed it is. `base` cannot depend on `profunctors`, but morally `Arrow` is equivalent to `Strong`+`Category`, because the typeclasses can be inter-defined like so:
```hs
instance Arrow q => Profunctor q where
  dimap f g h = arr g . h . arr f
instance Arrow q => Strong q where
  first' = first
  second' = second

instance (Strong q, Category q) => Arrow q where
  arr f = dimap f id id
  first = first'
  second = second'
```
Under this identification we see that `fanout` is actually equal to `&&&`, and the following is sufficient to define `Arrow`:
```hs
instance (Profunctor p, Category p) => Arrow (Bulk p) where
  arr f = dimap f id id
  first = first'
  second = second'
```
Note that if we instead defined `arr f = Bulk (arr (fmap f))`, this would unnecessarily require `Strong p`.

## Another Take at Scalability

Equipped with `Arrow`, `Traversing`, etc typeclass methods as compositional glue combinators, we can take another stab at defining a large query out of smaller pieces:
```hs
userDataByID :: Bulk (Kleisli IO) UserID UserData
orderDataByID :: Bulk (Kleisli IO) OrderID OrderData
productDataByID :: Bulk (Kleisli IO) ProductID ProductData
paymentDataByID :: Bulk (Kleisli IO) PaymentID PaymentData
orderIDsByUserID :: Bulk (Kleisli IO) UserID [OrderID]
productIDsByOrderID :: Bulk (Kleisli IO) OrderID [ProductID]
paymentIDsByOrderID :: Bulk (Kleisli IO) OrderID [PaymentID]

usersEverything
  :: Bulk (Kleisli IO)
    UserID
    (UserData, [(OrderData, [ProductData], [PaymentData])])
usersEverything
  = userDataByID
    &&& (traverse' orderEverything . orderIDsByUserID)
  where
    orderEverything = dimap id (\(x, (y, z)) -> (x, y, z))
      $ orderDataByID
        &&& (traverse' productDataByID . productIDsByOrderID)
        &&& (traverse' paymentDataByID . paymentIDsByOrderID)
```
Much better. What's the type of `orderEverything`?
```hs
orderEverything
  :: Bulk (Kleisli IO)
    OrderID
    (OrderData, [ProductData], [PaymentData])
```

Now you can compose these bulk processes pretty much the same way as you can compose ordinary functions, except you're forced to use point-free style, because you can't give names to intermediate values using lambda binders.

Or can you? With the `Arrow` typeclass comes the `Arrows` extension which normally gets very little use, but works perfectly here. Just like a `do`-block lets you give names to results of monadic actions, a `proc` block lets you give names to values flowing in a string diagram, which is then "compiled" down to `Arrow` operations.

Here's what the function looks like using arrow syntax:
```hs
usersEverything
  :: Bulk (Kleisli IO)
    UserID
    (UserData, [(OrderData, [ProductData], [PaymentData])])
usersEverything = proc userID -> do
  userData <- userDataByID -< userID
  orderIDs <- orderIDsByUserID -< userID
  orders <- traverse' orderEverything -< orderIDs
  returnA -< (userData, orders)
  where
    orderEverything = proc orderID -> do
      orderData <- orderDataByID -< orderID
      productIDs <- productIDsByOrderID -< orderID
      paymentIDs <- paymentIDsByOrderID -< orderID
      products <- traverse' productDataByID -< productIDs
      payments <- traverse' paymentDataByID -< paymentIDs
      returnA -< (orderData, products, payments)
```
With all intermediate values named, the code is self-documenting, you can add type signatures to the intermediate values, do some (irrefutable) pattern matching, construct return values (normally you'd probably be using a record type instead of this `(,,)` triple).

As an exercise to the reader, you can figure out the connection between [`Choice`](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor-Choice.html#t:Choice) and [`ArrowChoice`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Arrow.html#t:ArrowChoice), how `traverse'` gives you `right'` for free, and what that lets you do in terms of arrow syntax and DB queries.

## The `profunctors` Package

The `Bulk` type actually exists in the `profunctors` package as `CofreeTraversing`, and there is a `WrappedArrow` newtype to evidence that an `Arrow` is `Strong`, and that an `ArrowChoice` is `Choice`, but it's lacking a mechanism to observe the converse, i.e. that `CofreeTraversing` can be used with arrow syntax. However it is simple enough to add a blanket newtype wrapper that would translate `Strong` into `Arrow` and so on.

## Appendix

A note about query performance: if we're talking to a real database, the implementation of, say, `userDataByID` will have to build something like a `Map` or `HashMap` of values the DB has returned, in order to build the resulting traversable. `usersEverything` is composed out of multiple such functions, and in essence what happens is we send several queries to the DB, and glue their results together using `Map` or `HashMap`. This is essentially like executing a `JOIN` query using a Merge Join or a Hash Join strategy, except that the Merge/Hash Join part has been moved out of the DB's query planner and into our application. This has the disadvantage of depriving the query planner of the possibility to make an optimal choice based on the exact query and the data. Taking full advantage of the query planner would require making an SQL query builder -- definitely possible but a little outside the scope of this post.

Also, beware of long `IN ('x', 'y', ...)` lists, instead consider a [`VALUES` list](https://www.postgresql.org/docs/16/queries-values.html) (which can be `IN`'d, or `JOIN`'ed with).

