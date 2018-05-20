module Data.Smash.Env
  ( ask
  , askWith
  ) where

import Control.Comonad (extract)
import Control.Comonad.Env.Class as Env
import Data.Functor.Pairing.Co (Co)
import Data.Smash as S
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Proxy (Proxy2)
import Type.Row (class RowToList)

askWith
  :: forall l w r rl rest a
   . Env.ComonadEnv a w
  => IsSymbol l
  => RowCons l (Proxy2 w) rest r
  => RowToList rest rl
  => S.ComonadSmash rl rest
  => SProxy l
  -> Co (S.Smash r) a
askWith l = S.cosmash l \wa -> extract wa (Env.ask wa)

ask
  :: forall w r rl a
   . Env.ComonadEnv a w
  => RowToList r rl
  => S.ComonadSmash rl r
  => Co (S.Smash (env :: Proxy2 w | r)) a
ask = askWith (SProxy :: SProxy "env")
