## Module Data.Smash.Cofree

#### `lift`

``` purescript
lift :: forall f r rl a. RowToList r rl => Functor f => ComonadSmash rl r => Co f a -> Co (Smash (cofree :: Proxy2 (Cofree f) | r)) a
```

#### `liftWith`

``` purescript
liftWith :: forall l f r rl rest a. IsSymbol l => Functor f => Cons l (Proxy2 (Cofree f)) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> Co f a -> Co (Smash r) a
```


