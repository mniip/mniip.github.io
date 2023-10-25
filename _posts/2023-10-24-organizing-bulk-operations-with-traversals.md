---
title: Organizing Bulk Operations with Traversals
---

## The Problem

Suppose, in Haskell pseudocode, you have some process:
```hs
process :: A -> IO B
```
where `A` and `B` are arbitrary types. And suppose the implementation of the process is such that it is more efficient to do in bulk: you would provide several `A`'s upfront and would receive several `B`'s.

The question is, what type should you give to `processBulk`, the *bulk* version of `process`?

A lot of the time the need for such bulk operations arises because you're interacting with another system, crossing some sort of expensive API boundary. You could be talking to an SQL database, or a REST API if it has been thoughtfully extended with bulk endpoints. This API has its own signature, if not exactly a "type" then at least a pattern of how data flows in and out. It may be tempting to provide a thin wrapper around this pattern, and it's common to see types like:

- `[A] -> IO [B]` --- here we have a bespoke invariant that the resulting list has the same length as the input list, and that the items are in the same order.
- `[(Nonce, A)] -> IO [(Nonce, B)]` or, with a clearer expression of intent, `Map Nonce A -> IO (Map Nonce B)` --- here we have a bespoke invariant that all keys present in the input map must also be present in the output map. Or maybe you're intending that missing keys correspond to B's that don't exist? Then what you really have is `process :: A -> IO (Maybe B)`.
- `[A] -> IO [(A, B)]` or `Set A -> IO (Map A B)` if maybe `A` is small enough to act as its own nonce. This would be your average "select multiple things by their IDs" SQL query. And we have a similar bespoke invariant here.

In all cases we are faced with an invariant that is not captured by the type, and if you care about the correspondence between the input items and the output items, you may have trouble convincing the compiler that there is any. Say you chose `processBulk :: [A] -> IO [B]`, and then you wanted to simplify and define `process` in terms of `processBulk`? Well now you're faced with a partial function:
```hs
process :: A -> IO B
process a = processBulk [a] <&> \case
  [b] -> b
  _ -> error "???"
```

Perhaps `processBulk` is an internal function and you can look at what its use sites are. Some use sites might need to convert to one of the three aforementioned types anyway, and these conversions are fragile and error-prone, so why not hide it under the hood of `processBulk`. And if you have several use sites that require different formats you're just screwed I guess?

## An Aside

If you're not convinced by the utility of "optimizing" something that already works (why not just call `process` multiple times?), there are examples where bulk queries make a semantic difference.

Consider a procedure that does glob matching of multiple related files at once. Instead of asking for a random file that matches `*.hi` and a random file that matches `*.o`, you want a pair of files that match `$NAME.hi` and `$NAME.o` respectively, with a common string substituted for `$NAME`.

The same issue of returning a collection of outputs somehow connected to the collection of inputs arises here, and it's actually semantically different from iterating over inputs one by one.

## A Solution

You may take the bespoke invariant challenge literally, and decide to use some sort of length-indexed vector to lift the exact invariant to the type level:
```hs
processBulk :: forall (n :: Nat), Vector n A -> IO (Vector n B)

process :: A -> IO B
process a = processBulk (Cons a Nil) <&> \case
  Cons b Nil -> b
  -- pattern match is complete!
```
or maybe a [justified container](https://hackage.haskell.org/package/justified-containers):
```hs
processBulk :: JM.Map ph nonce A -> IO (JM.Map ph nonce B)

process :: A -> IO B
process a = JM.withSingleton () a \(k, m) ->
  process m <&> \m' -> m' JM.! k
  -- m' :: JM.Map ph () B;  k :: JM.Key ph ();  (JM.!) is total
```

These can be downgraded back to their respective ordinary list and map versions if needed, but these do not in any way help with converting between lists and maps, which is still error-prone and possibly partial.

## A Better Solution

The power of Haskell's type system allows us to write down a type that (with minor tweaks) is a generalization of all of the above, and more:
```hs
processBulk :: forall t. Traversable t => t A -> IO (t B)
```
You can straightforwardly specialize it with:
- `t ~ []`
- `t ~ Map Nonce`
- `t ~ Compose [] ((,) Nonce)`
- `t ~ Vector n`
- `t ~ Map ph Nonce`
- `t ~ Identity`

You can also recover the "self-keyed" flavors with:
```hs
processBulk . Map.fromSet id
  :: Set A -> IO (Map A B)

fmap getCompose . processBulk . Compose . map (join (,))
  :: [A] -> IO [(A, B)]
```

In cases where a bulk operation really is strictly an optimization of single operations, you can document this claim very concisely as a property test: `processBulk == traverse process`.

The implementation of such a function is actually relatively straightforward for the two kinds of "underlying interfaces" we've mentioned so far:
```hs
processList :: [A] -> IO [B]

processBulk :: Traversable t => t A -> IO (t B)
processBulk as = processList (toList as) <&> \bs ->
  case mapAccumL go bs as of
    ([], res) -> res
    (_, _) -> error "processList returned too many items"
  where
    go :: [B] -> A -> ([B], B)
    go (b:bs) _ = (bs, b)
    go [] _ = error "processList returned too few items"
```

```hs
processMap :: Map Nonce A -> IO (Map Nonce B)
randomNonce :: IO Nonce

processBulk :: Traversable t => t A -> IO (t B)
processBulk as = do
  tagged <- for as \a -> do
    nonce <- randomNonce
    pure (nonce, a)
  m <- processMap (Map.fromList $ toList tagged)
  pure $ tagged <&> \(nonce, _) -> m Map.! nonce
```

<details><summary>Precise exceptions versions:</summary>
{% highlight hs %}
processBulk :: Traversable t => t A -> IO (t B)
processBulk as = processList (toList as) >>= \bs ->
  runStateT (traverse (StateT . go) as) bs >>= \case
    (res, []) -> pure res
    (_, _) -> fail "processList returned too many items"
  where
    go :: A -> [B] -> IO (B, [B])
    go _ (b:bs) = pure (b, bs)
    go _ [] = fail "processList returned too few items"
{% endhighlight %}
{% highlight hs %}
processBulk :: Traversable t => t A -> IO (t B)
processBulk as = do
  tagged <- for as \a -> do
    nonce <- randomNonce
    pure (nonce, a)
  m <- processMap (Map.fromList $ toList tagged)
  for tagged \(nonce, _) -> case Map.lookup nonce m of
    Just b -> pure b
    Nothing -> fail "processMap didn't return a key we've given it"
{% endhighlight %}
</details>

You will notice something here: in order to satisfy the type of `processBulk` we are forced to eagerly check that the expected invariant holds. This is good because in the event that `processList`/`processMap` are some sort of external interface, and that interface behaves unexpectedly --- you'd rather find out about it early, than pass it to a Rube Goldberg machine of unchecked conversions that will just unexpectedly die at a random spot.

Even if your underlying interface and your use sites agree on e.g. using lists, and they don't break terribly if the invariant is violated so you think you can afford not to check --- even then I argue it is at least a good code organization measure. You have a type that clearly indicates intent: give me any structure with `A`'s and I'll fill it with `B`'s instead. You have a clear separation of responsibilities: it is `processBulk`'s job to validate the invariant. You have a flexible building block that can be easily repurposed in case the use sites change how they want to see the data.

I don't believe I am the first to discover this idea, at least I've been informed that @phadej [was writing](https://oleg.fi/gists/posts/2023-10-12-use-traversals-for-batch-operations.html) about essentially the same thing. Still, other than that I don't think I've seen another Haskell programmer talk about this technique, and I've definitely seen a lot of "bad" versions of `processBulk`.

## Appendix: Lens

It is possible to further generalize by abstracting out the type of `traverse`:
```hs
processBulkOf :: Traversal s t A B -> s -> IO t

processBulk = processBulkOf traverse
```

In this case the hypothetical property test becomes `processBulkOf == (%%~ process)`. An implementation of `processBulkOf` is left as an exercise to the reader. Note that the `mapAccumL`/`StateT` business is essentially what `unsafePartsOf` is.

This gets you the last 1% of flexibility over the 99% that `Traversable` gives you, but I would double check whether it's really needed.
