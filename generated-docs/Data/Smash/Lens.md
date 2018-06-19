## Module Data.Smash.Lens

#### `label`

``` purescript
label :: forall l s t a b rest. IsSymbol l => Functor b => Cons l (Proxy2 a) rest s => Cons l (Proxy2 b) rest t => SProxy l -> Lens (Smash s) (Smash t) a b
```

A `Lens` which focuses on the specified label in a `Smash` product.

#### `lowerOf`

``` purescript
lowerOf :: forall s t a. Functor a => Comonad t => Optic (Split a) s t a Identity -> s ~> a
```

Lower to a comonad identified by an optic.

For example:

```purescript
lowerOf first
  :: forall g f
   . Functor f
  => Comonad g
  => Day f g ~> f

lowerOf (first <<< second)
  :: forall f g h
   . Functor g
  => Comonad f
  => Comonad h
  => Day (Day f g) h ~> g
```

#### `liftCoOf`

``` purescript
liftCoOf :: forall s t a. Functor a => Comonad t => Optic (Split a) s t a Identity -> (Co a) ~> (Co s)
```

Lift an action in the `Co` monad over the specified optic.

For example:

```purescript
liftCoOf first
  :: forall g f
   . Functor f
  => Comonad g
  => Co f ~> Co (Day f g)

liftCoOf (first <<< second)
  :: forall f g h
   . Functor g
  => Comonad f
  => Comonad h
  => Co g ~> Co (Day (Day f g) h)
```

#### `Split`

``` purescript
newtype Split r f (g :: Type -> Type)
  = Split (f ~> g ⊗ r)
```

Split a functor into a Day convolution.
This is an implementation detail of the `lowerOf` function.

##### Instances
``` purescript
Profunctor (Split r)
Strong (Split r)
```


