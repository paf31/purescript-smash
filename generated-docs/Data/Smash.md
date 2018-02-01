## Module Data.Smash

#### `Smash`

``` purescript
data Smash (r :: # Type) (a :: Type)
```

Smash a bunch of functors together with Day convolution

The representation is abstract but can be understood in terms of
the defintion of `Day`:

```purescript
Day f g a ~ ∃x y. (f x, g y, x -> y -> a)
```

So for a smash product of several functors, we need a supply xⱼ of
existential variables, and then we define

```purescript
Smash f a = ∃x1 ... xⱼ. (Πⱼ fⱼ xⱼ,, Πⱼ xⱼ -> a)
```
which we represent using a record.

##### Instances
``` purescript
Functor (Smash r)
(RowToList r rl, ExtendSmash rl r) => Extend (Smash r)
(RowToList r rl, ComonadSmash rl r) => Comonad (Smash r)
```

#### `empty`

``` purescript
empty :: forall a. a -> Smash () a
```

Construct a value of type `Smash ()` by lifting a value of type `a`.

#### `singleton`

``` purescript
singleton :: forall l r f a. IsSymbol l => RowCons l (FProxy f) () r => SProxy l -> f a -> Smash r a
```

Construct a value of type `Smash (l :: FProxy f)` by lifting a value
of type `f a`.

#### `cons`

``` purescript
cons :: forall l f r1 r2 a b c. IsSymbol l => RowCons l (FProxy f) r1 r2 => SProxy l -> (a -> b -> c) -> f a -> Smash r1 b -> Smash r2 c
```

Add an interpreter of type `f a` to form a larger `Smash` product of
interpreters.

#### `uncons`

``` purescript
uncons :: forall l f r rest a. IsSymbol l => RowCons l (FProxy f) rest r => SProxy l -> Smash r a -> Exists (Uncons f rest a)
```

#### `lower`

``` purescript
lower :: forall l f r rl rest a. IsSymbol l => Functor f => RowCons l (FProxy f) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> Smash r a -> f a
```

Project out the interpreter at the specified label, ignoring the future
state of the other interpreters.

#### `smash`

``` purescript
smash :: forall interpreters results proxies rl. RowToList interpreters rl => Smashed rl interpreters proxies results => {  | interpreters } -> Smash proxies ({  | results })
```

Smash together a record of interpreters to get an interpreters which
returns records.

#### `cosmash`

``` purescript
cosmash :: forall l f r rest rl a. IsSymbol l => RowCons l (FProxy f) rest r => Functor f => RowToList rest rl => ComonadSmash rl rest => SProxy l -> (forall x. f (a -> x) -> x) -> Co (Smash r) a
```

A helper function for constructing actions in a `Co` monad.

#### `cosmash_`

``` purescript
cosmash_ :: forall l f r rest rl. IsSymbol l => RowCons l (FProxy f) rest r => Functor f => RowToList rest rl => ComonadSmash rl rest => SProxy l -> (forall x. f x -> x) -> Co (Smash r) Unit
```

A simpler variant of `cosmash` for when you don't care about the result.

#### `FProxy`

``` purescript
data FProxy (f :: Type -> Type)
  = FProxy
```

A value-level representation of a functor, so that we can use
some mono-kinded compiler-provided machinery like `RowCons`.

##### Instances
``` purescript
(Extend f, IsSymbol l, RowCons l (FProxy f) r1 r, ExtendSmash rl r1) => ExtendSmash (Cons l (FProxy f) rl) r
(Comonad f, IsSymbol l, RowCons l (FProxy f) r1 r, ComonadSmash rl r1) => ComonadSmash (Cons l (FProxy f) rl) r
```

#### `Uncons`

``` purescript
data Uncons f r a x
  = Uncons (f x) (Smash r (x -> a))
```

The result of extracting a single interpreter from a `Smash` product.

#### `Smashed`

``` purescript
class Smashed rl interpreters proxies results | rl -> interpreters proxies results where
  smashRL :: RLProxy rl -> {  | interpreters } -> Smash proxies ({  | results })
```

##### Instances
``` purescript
Smashed Nil () () ()
(IsSymbol l, RowLacks l results_, RowLacks l interpreters_, RowLacks l proxies_, RowCons l (f a) interpreters_ interpreters, RowCons l (FProxy f) proxies_ proxies, RowCons l a results_ results, Smashed rl interpreters_ proxies_ results_) => Smashed (Cons l (f a) rl) interpreters proxies results
```

#### `ExtendSmash`

``` purescript
class ExtendSmash rl r | rl -> r where
  duplicateSmashRL :: forall a. RLProxy rl -> Smash r a -> Smash r (Smash r a)
```

##### Instances
``` purescript
ExtendSmash Nil ()
(Extend f, IsSymbol l, RowCons l (FProxy f) r1 r, ExtendSmash rl r1) => ExtendSmash (Cons l (FProxy f) rl) r
```

#### `ComonadSmash`

``` purescript
class (ExtendSmash rl r) <= ComonadSmash rl r | rl -> r where
  extractSmashRL :: forall a. RLProxy rl -> Smash r a -> a
```

##### Instances
``` purescript
ComonadSmash Nil ()
(Comonad f, IsSymbol l, RowCons l (FProxy f) r1 r, ComonadSmash rl r1) => ComonadSmash (Cons l (FProxy f) rl) r
```


