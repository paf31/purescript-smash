module Test.Main where

import Prelude

import Control.Comonad (extract)
import Control.Comonad.Store (Store, store)
import Control.Comonad.Store.Class (class ComonadStore, pos, peek)
import Control.Extend (extend)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Functor.Pairing.Co (Co, pairCo)
import Data.Smash as S
import Data.Symbol (SProxy(..))
import Type.Row (class RowToList)

test1 :: S.Smash (foo :: S.FProxy (Store Int)) Int
test1 = S.singleton (SProxy :: SProxy "foo") (store (_ + 1) 1337)

test2 :: S.Smash (foo :: S.FProxy (Store Int), bar :: S.FProxy (Store Int)) Int
test2 = S.smash (SProxy :: SProxy "bar") (+) (store (_ * 2) 42) test1

get
  :: forall w r rl s
   . ComonadStore s w
  => RowToList r rl
  => S.ComonadSmash rl r
  => Co (S.Smash (foo :: S.FProxy w | r)) s
get = S.cosmash (SProxy :: SProxy "foo") \wa -> extract wa (pos wa)

put
  :: forall w r rl s
   . ComonadStore s w
  => RowToList r rl
  => S.ComonadSmash rl r
  => s
  -> Co (S.Smash (foo :: S.FProxy w | r)) Unit
put val = S.cosmash_ (SProxy :: SProxy "foo") (peek val)

test3
  :: forall r rl rl1
   . RowToList (foo :: S.FProxy (Store Int) | r) rl1
  => S.ExtendSmash rl1 (foo :: S.FProxy (Store Int) | r)
  => RowToList r rl
  => S.ComonadSmash rl r
  => Co (S.Smash (foo :: S.FProxy (Store Int) | r)) Unit
test3 = do
  i <- get
  put (i + 1)

main :: Eff (console :: CONSOLE) Unit
main = do
  logShow (extract test1)
  logShow (extract (extend extract test1))

  logShow (extract test2)
  logShow (extract (extend extract test2))

  logShow (pairCo (\i _ -> i) test1 test3)
  logShow (pairCo (\i _ -> i) test2 test3)
