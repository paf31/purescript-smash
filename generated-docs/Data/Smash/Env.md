## Module Data.Smash.Env

#### `ask`

``` purescript
ask :: forall w r rl a. ComonadEnv a w => RowToList r rl => ComonadSmash rl r => Co (Smash (env :: FProxy w | r)) a
```

#### `askWith`

``` purescript
askWith :: forall l w r rl rest a. ComonadEnv a w => IsSymbol l => RowCons l (FProxy w) rest r => RowToList rest rl => ComonadSmash rl rest => SProxy l -> Co (Smash r) a
```


