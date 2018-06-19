module Test.Main where

import Prelude

import Control.Comonad.Env (Env, env)
import Control.Comonad.Store (Store, store)
import Control.Comonad.Traced (Traced, traced)
import Data.Foldable (traverse_)
import Data.Functor.Pairing.Co (Co, pairCo)
import Data.Smash as S
import Data.Smash.Env (ask)
import Data.Smash.Store (get, put)
import Data.Smash.Traced (tell)
import Effect (Effect)
import Effect.Console (logShow)
import Type.Proxy (Proxy2)

script :: Co (S.Smash ( store :: Proxy2 (Store Int)
                      , traced :: Proxy2 (Traced (Array String))
                      , env :: Proxy2 (Env Boolean)
                      )) Unit
script = do
  increment <- ask
  i <- get
  tell ["The state is " <> show i]
  put (i + if increment then 1 else (-1))
  j <- get
  tell ["The state is now " <> show j]

main :: Effect Unit
main = do
  let interpreter = S.smash
        { store: store identity 1337
        , traced: traced identity
        , env: env true unit
        }
      { store, traced } = pairCo const interpreter script
  traverse_ logShow traced
  logShow store
