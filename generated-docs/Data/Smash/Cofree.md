## Module Data.Smash.Cofree

#### `lift`

``` purescript
lift :: forall f r rl a. RowToList r rl => Functor f => ComonadSmash rl r => Co f a -> Co (Smash (cofree :: FProxy (Cofree f) | r)) a
```

#### `liftWith`

``` purescript
liftWith :: forall l f r rl rest a. IsSymbol l => Functor f => RowCons l (FProxy (Cofree f)) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> Co f a -> Co (Smash r) a
```


