module Data.Smash.Store
  ( get
  , getWith
  , put
  , putWith
  ) where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store.Class (class ComonadStore, pos, peek)
import Data.Functor.Pairing.Co (Co)
import Data.Smash as S
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Proxy (Proxy2)
import Type.Row (class RowToList)

getWith
  :: forall l w r rl rest a
   . ComonadStore a w
  => IsSymbol l
  => RowCons l (Proxy2 w) rest r
  => RowToList rest rl
  => S.ComonadSmash rl rest
  => SProxy l
  -> Co (S.Smash r) a
getWith l = S.cosmash l \wa -> extract wa (pos wa)

get
  :: forall w r rl a
   . ComonadStore a w
  => RowToList r rl
  => S.ComonadSmash rl r
  => Co (S.Smash (store :: Proxy2 w | r)) a
get = getWith (SProxy :: SProxy "store")

putWith
  :: forall l w r rl rest a
   . ComonadStore a w
  => IsSymbol l
  => RowCons l (Proxy2 w) rest r
  => RowToList rest rl
  => S.ComonadSmash rl rest
  => SProxy l
  -> a
  -> Co (S.Smash r) Unit
putWith l val = S.cosmash_ l (peek val)

put
  :: forall w r rl a
   . ComonadStore a w
  => RowToList r rl
  => S.ComonadSmash rl r
  => a
  -> Co (S.Smash (store :: Proxy2 w | r)) Unit
put = putWith (SProxy :: SProxy "store")
