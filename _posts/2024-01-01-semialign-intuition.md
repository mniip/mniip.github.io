---
title: From Semialign to Crosswalk
---

* TOC
{:toc}

## Shapes

As explained in the [official docs for `Traversable`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Traversable.html#g:4), a traversable functor can be intuitively decomposed into a "shape" and a list of "elements":
```hs
type Shape t = t ()
shape :: Functor t => t a -> Shape t
shape xs = () <$ xs

decompose :: Traversable t => t a -> (Shape t, [a])
decompose xs = (shape xs, toList xs)
```

We can even recombine the shape and the elements together, but note that `length (shape xs) = length xs = length (toList xs)`, and this becomes a requirement for being able to recombine:
```hs
recombine :: Traversable t => (Shape t, [a]) -> t a
recombine (s, elems) =
  case mapAccumL (\(x:xs) () -> (xs, x)) elems s of
    ([], res) -> res
```
In a dependently typed language we could precisely encode this invariant with a dependent sum/dependent pair and a length-indexed vector:
```hs
type Decomposition t a = { shape :: Shape t | Vector (length shape) a }
```
In fancy math terms, we have a *fibration* `shape :: t a -> Shape t`, whose *total space* is `t a`, and its *base space* is `Shape t`. Given a specific shape `s :: Shape t`, we have the *fiber over `s`*: a subset of exactly those elements of `t a` that have shape `s`. As the above decomposition shows, this fiber is isomorphic to `Vector (length s) a`, i.e. a product of exactly `length s` copies of `a`.

Examples:
- If `t ~ []`, we have `Shape []` isomorphic to `Natural` --- the shape of a list is uniquely determined by its length.
- If `t ~ Map k`, then `Shape (Map k)` is isomorphic to `Set k` --- the shape of a map is its set of keys. Note that in this case multiple shapes can have the same length.
- If `t ~ Maybe`, then there are only two shapes: a shape for `Nothing`, of length 0; and a shape for `Just`, of length 1. There are no shapes of length 2 or more.

The function `length` encodes a relationship between shapes and natural numbers. In general it can be arbitrary: given some set of shapes `S` and some function `l :: S -> Natural`, we have a container:
```hs
type C a = { s :: S | Vector (l s) a }
````
such that `Shape C` is isomorphic to `S`, and `length = l` (via the aforementioned isomorphism).

For a general traversable functor you know nothing about how its space of shapes is structured, and the `Traversable` interface is centered around the notion of *preserving* the shape: you work on one "container" at a time, and whatever shape comes in, also comes out. To work with multiple containers at once, we need additional structure to talk about how their shapes interact.

## Unions

A problem often occurring in the real world is that of zipping lists together. There's a function called `Prelude.zip` that tries to address this problem, but for lists of uneven length it truncates at the shorter list, and silently ignores the tail of the longer list. For some tasks this is unsuitable, and instead you want to take the longest of the two lists, padding the shorter one in some manner. As far as I can understand, this is the original problem that [`these`](https://hackage.haskell.org/package/these) and later [`semialign`](https://hackage.haskell.org/package/semialign) set out to solve.

There we see things like:
```hs
data These a b = This a | That b | These a b

class Semialign t where
  align :: t a -> t b -> t (These a b)

instance Semialign []
instance Ord k => Semialign (Map k)
```

It may be tempting to assume that we can simply take the "longer" container, and then combine the elements by padding the "shorter" container. But, say, in case of `Map k` that's not even well defined. For `Map.singleton 'x' 3` and `Map.singleton 'y' 2`, neither can be said to be "longer" than the other, and what we probably want to get out of combining them is actually `Map.fromList [('x', This 3), ('y', That 2)]`.

If we focus on what happens to the *shapes* of the containers:
```hs
join :: Semialign t => Shape t -> Shape t -> Shape t
join s u = () <$ align s u
```
we see that we've actually made up a binary operation that is:
- *commutative*: `join s u = join u s`,
- *associative*: `join s (join u v) = join (join s u) v`,
- and *idempotent*: `join s s = s`.

For `[]`, whose shapes are indentified with natural numbers, this is the operation of taking the maximum of two numbers. For `Map k`, it is the operation of taking the union of two sets.

What's left is figuring out what happens to the elements. We've combined a container of shape `s` with `length s` elements and a container of shape `u` with `length u` elements, to obtain a container of shape `join s u` with `length (join s u)` elements. So how are the input elements related to the output elements? Parametricity forbids us from actually changing the element values, so we are restricted to talking about *locations* of the elements.

We expect that the shape `join s u` has locations that were there in `s`, locations that were there in `u`, and locations that were there in both; but not locations that weren't in either. This is enforced by giving `align` the following type:
```hs
align :: t a -> t b -> t (These a b)
```

In addition to the commutativity, associativity, and idempotency properties that can be expressed using just shapes, we have to preserve the structure of the elements. Elements form an ordered list, and `align` preserves this structure. As we map locations from `s` to `join s u`, we don't lose or duplicate any, and we preserve their order:
```hs
toList xs = mapMaybe here (toList (align xs ys))
  where
    here :: These a b -> Maybe a
    here (This x) = Just x
    here (That _) = Nothing
    here (These x _) = Just x
```

The attribution of locations done by `align` can be demonstrated on a diagram like this:
<figure>
    <img src="/static/img/semialign-intuition/semialign_map_diagram.svg" alt="A diagram demonstrating the attribution of locations when semialign'ing two maps" />
</figure>

## Empty

Where there's a binary operation, there's a potential for having units. We want `nil :: Shape t` such that `join xs nil = xs`. For elements we expect a stronger property: an element of `align xs nil` is never attributed to an element of `nil`, thus:
```hs
align xs nil = This <$> xs
```
A way to enforce this using types is to say that actually `nil :: t Void`, and then `align xs nil :: t (These a Void)`, and `These a Void` is forced to be the `This` constructor. This implies that `nil` has no elements (`length nil = 0`).

The `semialign` library uses an equivalent formulation:
```hs
class Semialign t => Align t where
  nil :: t a
```

The left unit laws (`join nil ys = ys`, `align nil ys = That <$> ys`) follow from commutativity of `align`.

## Aside: Lattices

A [semilattice](https://en.wikipedia.org/wiki/Semilattice) is a structure that can be defined from two different perspectives:
- Order-theoretically, it is a partially ordered set with some relation `<=`, and it has *least upper bounds*: for any `x` and `y` there exists an "upper bound" `L` (`x <= L` and `y <= L`) that is "least" (if `x <= M` and `y <= M` then `L <= M`).
- Algebraically, it is a set with a binary operation `join` that is commutative, associative, and idempotent.
To connect the two we see that `join x y` is the least upper bound of `x` and `y`, and the relation `<=` is defined by:
```hs
x <= y  <=>  y = join x y
```

For example, the `max` operation on the naturals is connected to the ordinary `<=` relation on the naturals. The union of sets is connected to the relation of being a subset.

So partially ordered sets are a "superclass" of semilattices, but to observe this we have to forgo of the `join` binary operation and work with `<=` instead.

On shapes, `<=` is a relation and doesn't carry any data. With elements, we see that in the case that `s <= u`, we have to introduce the data that relates the locations in `s` and the locations in `u`. We expect that a location in `u` is related to 0 or 1 locations in `s`, and that the order is preserved. This can be captured as a pair of maps:
- An expansion map `Vector (length s) a -> Vector (length u) (Maybe a)` that doesn't drop or duplicate elements, but may skip over some locations in the output (filling them with `Nothing`).
- A restriction map `Vector (length u) a -> Vector (length s) a` that doesn't duplicate elements or skip over locations, but may drop elements.

In terms of the container type `t`, we could assign them types like:
```hs
expansion :: Shape t -> t a -> Maybe (t (Maybe a))
restriction :: Shape t -> t a -> Maybe (t a)
```
but these don't capture nearly enough invariants, which we have to state as additional axioms:
```hs
if expansion s xs = Just ys then:
  shape ys = s
if restriction s xs = Just ys then:
  shape ys = s
isJust (expansion (shape xs) ys)  <=>  isJust (restriction (shape ys) xs)
```

Here's a diagram demonstrating these, given `s = Set.fromList ['a']` and `u = Set.fromList ['a', 'b']`:
<figure>
    <img src="/static/img/semialign-intuition/exchange_map_diagram.svg" alt="A diagram demonstrating the attribution of locations when expanding or restricting a map" />
</figure>

<details><summary>Here's a better attempt:</summary>
{% highlight hs %}
class (Functor t, Foldable t) => PartiallyOrdered t where
  exchange :: (t a, t b) -> Maybe (t (b, Maybe a), t (a, b))
  -- If exchange (xs, ys) = Just (xs', ys') then:
  --   fst <$> xs' = ys  (corollary: shape xs' = shape ys)
  --   fst <$> ys' = xs  (corollary: shape ys' = shape xs)
  --   catMaybes (toList (snd <$> xs')) == toList xs
  --   toList (snd <$> ys) `isSubsequenceOf` toList ys
  -- If exchange (xs, ys) = Just (xs', _)
  -- and exchange (snd <$> xs', zs) = Just (xs'', _) then:
  --   exchange (xs, zs) = Just (xs'' <&> \(x, mmy) -> (x, join mmy), _)
  -- If exchange (ys, zs) = Just (_, zs')
  -- and exchange (xs, snd <$> zs') = Just (_, zs'') then:
  --   exchange (xs, zs) = Just (_, zs'')
  -- exchange (xs, xs) = Just (xs <&> \x -> (x, Just x), xs <&> \x -> (x, x))

isSubshape :: PartiallyOrdered t => Shape t -> Shape t -> Bool
isSubshape s u = isJust $ exchange (s, u)
-- corollary: 'isSubshape' is a partial order

expansion :: PartiallyOrdered t => Shape t -> t a -> Maybe (t (Maybe a))
expansion s xs = exchange (xs, s) <&> \(xs', _) -> snd <$> xs'
-- corollary: expansion (shape xs) xs = Just (Just <$> xs)
-- corollary: fmap join <$> (expansion s xs >>= expansion u) = expansion u

restriction :: PartiallyOrdered t => Shape t -> t a -> Maybe (t a)
restriction s xs = exchange (s, xs) <&> \(_, xs') -> snd <$> xs'
-- corollary: restriction (shape xs) xs = Just xs
-- corollary: restriction s xs >>= restriction u = restriction u xs
{% endhighlight %}
</details>

This superclass should have a compatibility law with `Semialign`:
```hs
expansion (shape (align xs ys)) xs = Just (here <$> align xs ys)
if expansion s xs = Just ys then:
  align s xs = maybe (This ()) (These ()) <$> ys
```

<details><summary>Nested aside: restriction</summary>
<code>Semialign</code> only captures the expansion part of the relation. To capture the restriction part too, we would need to change the type of <code>align</code> to something like:
{% highlight hs %}
align :: t a -> t b -> (t (These a b), t (a, Maybe b), t (Maybe a, b))
-- expand the two inputs to the union shape, but also restrict the union shape
-- to the two shapes of the inputs
{% endhighlight %}
</details>

## Intersections

Where there's a transitive relation, there's potential for duality. The opposite of a least upper bound is a *greatest lower bound*. That means for `x` and `y` there's a "lower bound" L (`L <= x` and `L <= y`) that is "greatest" (if `M <= x` and `M <= y` then `M <= L`).

Equivalently, it means there's a binary operation which we'll denote `meet` that is commutative, associative, and idempotent. How is it different from `join`? Well, it isn't really. But it has a different connection to the `<=` relation:
```hs
x <= y  <=>  x = meet x y
```
So by defining `join` and `meet` on the same set, we mean that they share the same `<=` relation, which actually means they are a kind of opposites:
```hs
x = meet x y  <=>  y = join x y
```
A more common way to write this is called the "absorption laws":
```hs
join x (meet x y) = x
meet x (join x y) = x
```
Once these are satified, the resulting combination of the join-semilattice and the meet-semilattice is called a *lattice*.

For example the opposite of `max` on naturals is `min`, and the opposite of unions of sets is intersection of sets. What does this mean for our containers? That we're defining an operation that takes the shorter of the two lists, or the intersection of two maps. Such operations are also useful quite often, and it helps that they are special cases of a general pattern. The `semialign` library defines a typeclass with a method named `zip` for this purpose.

For elements, just like `align` expands the two inputs to the smallest shape that can fit them both, `zip` would have to *restrict* the two inputs to the largest shape that fits inside them both. Restriction never skips over locations in the output, so the type is:
```hs
zip :: t a -> t b -> t (a, b)
```
Restriction may however drop input elements. But it does preserve order:
```hs
toList xs `isSubsetOf` toList (zip xs ys)
```

Restriction has compatibility laws with `zip`:
```hs
restriction (shape (zip xs ys)) xs = Just (fst <$> zip xs ys)
if restriction s xs = Just ys then:
  zip s xs = ((),) <$> ys
```

<details><summary>Nested aside 2: expansion</summary>
Dually, <code>Zip</code> doesn't capture the expansion part of the relation. We would need to modify <code>zip</code> to also return the input shapes with the intersection shape expanded into them.
</details>

## Distributivity

An additional requirement that `join` and `meet` can satisfy is *distributivity*. This is expressed as either of the two equivalent conditions:
```hs
join (meet s t) u = meet (join s u) (join t u)
meet (join s t) u = join (meet s u) (meet t u)
```
However, care must be taken with generalizing these properties from mere shapes to elements. Suppose `xs :: t a`, `ys :: t b`, `zs :: t c`, then:
```hs
align (zip xs ys) zs :: t (These (a, b) c)
zip (align xs zs) (align ys zs) :: t (These a c, These b c)
```
Suppose there's a shape `s` such that `s <= shape xs` and `s <= shape zs`, but `not (s <= shape ys)`. Then it follows that `not (s <= shape (zip xs ys))` and `s <= shape (align (zip xs ys) zs)`. This means that elements of `xs` that can be restricted to `s` will have been dropped when zipping with `ys`, and elements of `align (zip xs ys) zs` that can be restricted to `s` will never be attributed to anything from `xs`. On the other hand, we have `s <= shape (align xs zs)`, meaning `zip (align xs zs)` retains elements of `xs` that can be restricted to `s`, and thus so does `zip (align xs zs) (align ys zs)`.

In general, we can consult the truth table of `(X and Y) or Z`, which is equivalent to `(X or Z) and (Y or Z)`:
```
X Y Z | (X and Y) or Z
------+---------------
0 0 0 | 0
0 0 1 | 1
0 1 0 | 0
0 1 1 | 1
1 0 0 | 0
1 0 1 | 1
1 1 0 | 1
1 1 1 | 1
```
The `1` entries correspond to presence of elements, and `0`s correspond to absence. Thus the most general type of element that we can use for the result of combining 3 containers this way is a datatype with 5 constructors, which can be mapped to `These (a, b) c` and `(These a c, These b c)`:
```hs
data AndOr a b c
  = C c
  | BC b c
  | AC a c
  | AB a b
  | ABC a b c

toThesePair :: AndOr a b c -> These (a, b) c
toThesePair (C z) = That z
toThesePair (BC _y z) = That z
toThesePair (AC _x z) = That z
toThesePair (AB x y) = This (x, y)
toThesePair (ABC x y z) = These (x, y) z

toPairThese :: AndOr a b c -> (These a c, These b c)
toPairThese (C z) = (That z, That z)
toPairThese (BC y z) = (That z, These y z)
toPairThese (AC x z) = (These x z, That z)
toPairThese (AB x y) = (This x, This y)
toPairThese (ABC x y z) = (These x z, These y z)
```

Note the dropping and duplication of data, which doesn't let us express these as a simple function from `These (a, b) c` to `(These a c, These b c)` or the other way. Instead we have to assert that there exists some `rs :: t (AndOr a b c)` such that:
```hs
align (zip xs ys) zs = toThesePair <$> rs
zip (align xs ys) (align ys zs) = toPairThese <$> rs
```

The other distributive law, relating `zip (align xs ys) zs` and `align (zip xs zs) (zip ys zs)` turns out to be simpler. The truth table for `(X or Y) and Z` shows that the most general type of element is equivalent to `(These a b, c)`, which can be turned into `These (a, c) (b, c)` using `distrPairThese`, thus the law is:
```hs
distrPairThese <$> zip (align xs ys) zs = align (zip xs zs) (zip ys zs)
```

Distributivity actually adds a lot of rigidity into the structure of a lattice, that we can use to more easily reason about them. [Birkhoff's representation theorem](https://en.wikipedia.org/wiki/Birkhoff%27s_representation_theorem) states that every finite distributive lattice is a sublattice of the lattice of sets, under regular operations of union and intersection. This means that `t ~ Map k` is in a sense a universal example, and every other example is merely restricting `Map k` to a subset of shapes. For example, lists can be thought of as `Map Natural` with the restriction that keys have to start from `0` and be consecutive.

Equivalently, every distributive lattice is a sublattice of the product of some copies of the two-element lattice, which corresponds to `t ~ Maybe`. This means that a container type can be "factored" into individual locations, each of which can be filled with an element or not, and the "names" of these locations can be consistent between the different shapes of the container. For example, "3rd element of the list" is a location that makes sense for all lists, but in some lists it's merely not filled with an element.

<details><summary>Caveat: the factors need not have exactly 1 element</summary>
Birkhoff's representation theorem technically talks about join-irreducible elements of the lattice, which for containers means a shape that is not the union of non-<code>nil</code> shapes. This leads to the following couple of pathological examples:

{% highlight hs %}
instance Semialign (Const Bool) where align (Const x) (Const y) = Const (x || y)
instance Zip (Const Bool) where zip (Const x) (Const y) = Const (x && y)

data Maybe2 a = Just2 a a | Nothing2
instance Semialign Maybe2 -- self-evident
instance Zip Maybe2 -- self-evident
{% endhighlight %}

The lattice of shapes in both cases is isomorphic to the two-element lattice, but in the first case the join-irreducible lattice element is <code>Const True</code>, which has no elements; and in the second case the join-irreducible lattice element is <code>Just2</code>, which has 2 elements.
</details>

## Full

The `nil` shape is the unit of `join`, and equivalently, the *least element* in the (semi)lattice. Similarly, `meet` can have a unit, or equivalently, the *greatest element*. A lattice with a least and a greatest element is called a *bounded lattice*.

We can call this `full :: Shape t`. Then we expect that the result of zipping with `full` will keep every location, i.e. `full` itself has an element for every possible location in any other shape it could be zipped with.

The `semialign` library again opts for a Yoneda encoding:
```hs
class Zip f => Repeat f where
  repeat :: a -> f a
```
The two are related by `full = repeat ()` and `repeat x = x <$ full`.

For lists this corresponds to an infinitely long list (hence the name of the method). For maps this would be a map with all keys present, though this is only possible with a `containers` map if the key type is finite.

## Crosswalk

A common intuition for `Applicative` is that it allows us to define a family of ways to lift functions of arbitrary arity:
```hs
liftAN
  :: Applicative f
  => (a1 -> a2 -> ... -> an -> r) -> f a1 -> f a2 -> ... -> f an -> f r
liftAN f xs1 xs2 ... xsn = f <$> xs1 <*> xs2 <*> ... <*> xsn
```
Equivalently, we can commute `f` with a tuple of arbitrary width:
```hs
pairN :: Applicative f => (f a1, f a2, ... , f an) -> f (a1, a2, ..., an)
pairN (xs1, xs2, ... , xsn) = liftAN (,, ... ,,) xs1 xs2 ... xsn
```

Let's consider `Semialign` on the other hand. If we apply `align` twice to combine 3 containers, we end up with a type of elements like `These a (These b c)`. If we enumerate the possibilities, we can see that this type encodes a non-empty sub-tuple of `(a, b, c)`. In general, applying `align` multiple times to combine N containers will yield a container with some nesting of `These` that encodes a non-empty sub-tuple of N types:
```hs
theseN
  :: Align f
  => (f a1, f a2, ..., f an) -> f (These a1 (These a2 (... (These an Void) ...)))
theseN (xs1, xs2, ... , xsn) = align xs1 (align xs2 (... (align xsn nil) ...))
```

We can think of `sequenceA` as taking a container apart into its shape and list of N elements, then applying the applicative `pairN` to the elements, then fmapping the result with a function that combines the N elements and the shape back into the container. Analogously, we can envision a function that takes a container apart into its shape and list of N elements, then applies `theseN` to the elements, then fmaps the result with a function that reassembles the container. This function will receive only a non-empty subset of the elements, and so may need to change the shape of the container.

The `semialign` library calls this `Crosswalk`, and provides an interface similar to `Traversable`:
```hs
class (Functor t, Foldable t) => Crosswalk t where
  sequenceL :: Align f => t (f a) -> f (t a)
  crosswalk :: Align f => (a -> f b) -> t a -> f (t b)
```
The intuition behind this class should be that a crosswalk allows you to select a non-empty subset of the locations in a container. Remembering that `Maybe` is an `Align`, we can see this in action:
```hs
sequenceL @_ @Maybe :: Crosswalk t => t (Maybe a) -> Maybe (t a)
```
This is similar to `catMaybes` (or its generalization in [witherable](https://hackage.haskell.org/package/witherable-0.4.2/docs/Data-Witherable.html#v:catMaybes)), but with a twist: if the container is all `Nothing`s, we fail and return `Nothing`. As such, the container being crosswalked doesn't have to support being empty.

The prototypical examples of a `Crosswalk` are:
- `t ~ NonEmpty`: we can select a non-empty subset of locations, and after compacting the list by removing the holes we are guaranteed to have a non-empty list.
- `t ~ []`: if the list is empty we return `nil`, otherwise we behave just as in the case of `NonEmpty`.
- `t ~ Map k`: we select which locations (keys) to retain, and which to remove.

<details><summary><code>semialign</code> doesn't provide instances for <code>NonEmpty</code> and <code>Map k</code> so here they are</summary>
{% highlight hs %}
instance Crosswalk NonEmpty where
  crosswalk f (x NonEmpty.:| []) = NonEmpty.singleton <$> f x
  crosswalk f (x NonEmpty.:| y:zs)
    = alignWith assemble (f x) (crosswalk f (y NonEmpty.:| zs))
    where
      assemble (This x) = NonEmpty.singleton x
      assemble (That xs) = xs
      assemble (These x xs) = x NonEmpty.<| xs

instance Crosswalk (Map k) where
  crosswalk _ Map.Tip = nil
  crosswalk f (Map.Bin _ k v l r)
    = assemble <$> (crosswalk f l `align` f v `align` crosswalk f r)
    where
      assemble (This (This l')) = l'
      assemble (This (That v')) = Map.singleton k v'
      assemble (That r') = r'
      assemble (These (That v') r') = Map.insertMin k v' r'
      assemble (This (These l' v')) = Map.insertMax k v' l'
      assemble (These (This l') r') = Map.glue l' r'
      assemble (These (These l' v') r') = Map.link k v' l' r'
{% endhighlight %}
</details>

More generally, `sequenceL` can be thought of as a generalized `transpose`, but not the kind that you get by traversing with a `ZipList`, but rather [the one from Data.List](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#v:transpose), which can deal with lists of uneven length by skipping over holes.

Here's a diagram desmonstrating an example with `t ~ []` and `f ~ Map Char`:
<figure>
    <img src="/static/img/semialign-intuition/crosswalk_map_diagram.svg" alt="A diagram demonstrating selecting a subset of locations using crosswalk" />
</figure>
Note how for locations that never occur in the `f` (e.g. the `'d'` key), we don't produce a location in the result, thus we never need to construct an empty `t`.

Note that in general the `Crosswalk t` structure has nothing to with the `Semialign t` or the `Zip t` structure. For `t ~ Map k` they are compatible, but for lists `sequenceL` will skip over holes, and elements that follow holes will have been moved to different indices.
