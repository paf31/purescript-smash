## Module Data.Smash.Traced

#### `tell`

``` purescript
tell :: forall w r rl a. ComonadTraced a w => RowToList r rl => ComonadSmash rl r => a -> Co (Smash (traced :: Proxy2 w | r)) Unit
```

#### `tellWith`

``` purescript
tellWith :: forall l w r rl rest a. ComonadTraced a w => IsSymbol l => Cons l (Proxy2 w) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> a -> Co (Smash r) Unit
```


