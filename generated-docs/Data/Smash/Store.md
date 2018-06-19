## Module Data.Smash.Store

#### `get`

``` purescript
get :: forall w r rl a. ComonadStore a w => RowToList r rl => ComonadSmash rl r => Co (Smash (store :: Proxy2 w | r)) a
```

#### `getWith`

``` purescript
getWith :: forall l w r rl rest a. ComonadStore a w => IsSymbol l => Cons l (Proxy2 w) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> Co (Smash r) a
```

#### `put`

``` purescript
put :: forall w r rl a. ComonadStore a w => RowToList r rl => ComonadSmash rl r => a -> Co (Smash (store :: Proxy2 w | r)) Unit
```

#### `putWith`

``` purescript
putWith :: forall l w r rl rest a. ComonadStore a w => IsSymbol l => Cons l (Proxy2 w) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> a -> Co (Smash r) Unit
```


