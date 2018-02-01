module Test.Main where

import Prelude

import Control.Comonad.Env (Env, env)
import Control.Comonad.Store (Store, store)
import Control.Comonad.Traced (Traced, traced)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Foldable (traverse_)
import Data.Functor.Pairing.Co (Co, pairCo)
import Data.Smash as S
import Data.Smash.Env (ask)
import Data.Smash.Store (get, put)
import Data.Smash.Traced (tell)

script :: Co (S.Smash ( store :: S.FProxy (Store Int)
                      , traced :: S.FProxy (Traced (Array String))
                      , env :: S.FProxy (Env Boolean)
                      )) Unit
script = do
  increment <- ask
  i <- get
  tell ["The state is " <> show i]
  put (i + if increment then 1 else (-1))
  j <- get
  tell ["The state is now " <> show j]

main :: Eff (console :: CONSOLE) Unit
main = do
  let interpreter = S.smash
        { store: store id 1337
        , traced: traced id
        , env: env true unit
        }
      { store, traced } = pairCo const interpreter script
  traverse_ logShow traced
  logShow store
