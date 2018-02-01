## Module Data.Smash.Traced

#### `tell`

``` purescript
tell :: forall w r rl a. ComonadTraced a w => RowToList r rl => ComonadSmash rl r => a -> Co (Smash (traced :: FProxy w | r)) Unit
```

#### `tellWith`

``` purescript
tellWith :: forall l w r rl rest a. ComonadTraced a w => IsSymbol l => RowCons l (FProxy w) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> a -> Co (Smash r) Unit
```


