module Data.Smash.Traced
  ( tell
  , tellWith
  ) where

import Prelude

import Control.Comonad.Traced.Class (class ComonadTraced, track)
import Data.Functor.Pairing.Co (Co)
import Data.Smash as S
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row (class RowToList)

tellWith
  :: forall l w r rl rest a
   . ComonadTraced a w
  => IsSymbol l
  => RowCons l (S.FProxy w) rest r
  => RowToList rest rl
  => S.ComonadSmash rl rest
  => SProxy l
  -> a
  -> Co (S.Smash r) Unit
tellWith l val = S.cosmash_ l (track val)

tell
  :: forall w r rl a
   . ComonadTraced a w
  => RowToList r rl
  => S.ComonadSmash rl r
  => a
  -> Co (S.Smash (traced :: S.FProxy w | r)) Unit
tell = tellWith (SProxy :: SProxy "traced")
