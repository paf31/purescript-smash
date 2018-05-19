module Data.Smash.Cofree
  ( lift
  , liftWith
  ) where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree, tail)
import Data.Functor.Pairing.Co (Co, runCo)
import Data.Smash as S
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Proxy (Proxy2)
import Type.Row (class RowToList)

liftWith
  :: forall l f r rl rest a
   . IsSymbol l
  => Functor f
  => RowCons l (Proxy2 (Cofree f)) rest r
  => RowToList rest rl
  => S.ComonadSmash rl rest
  => SProxy l
  -> Co f a
  -> Co (S.Smash r) a
liftWith l fa = S.cosmash l (runCo fa <<< map extract <<< tail)

lift
  :: forall f r rl a
   . RowToList r rl
  => Functor f
  => S.ComonadSmash rl r
  => Co f a
  -> Co (S.Smash (cofree :: Proxy2 (Cofree f) | r)) a
lift = liftWith (SProxy :: SProxy "cofree")
