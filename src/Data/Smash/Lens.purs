module Data.Smash.Lens
  ( label
  , lowerOf
  , liftCoOf
  , Split(..)
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Data.Exists (runExists)
import Data.Functor.Day (type (⊗), assoclDay, assocrDay, day, hoistDay1, hoistDay2, introDay1, runDay, symmDay)
import Data.Functor.Day.Hom (Hom, hom)
import Data.Functor.Day.Profunctor (class Profunctor, class Strong, Lens, Optic, lens)
import Data.Functor.Pairing.Co (Co, co, runCo)
import Data.Identity (Identity)
import Data.Smash (Smash, Uncons(..), cons, uncons)
import Data.Symbol (class IsSymbol, SProxy)
import Prim.Row as Row
import Type.Proxy (Proxy2)

-- | A `Lens` which focuses on the specified label in a `Smash` product.
label
  :: forall l s t a b rest
   . IsSymbol l
  => Functor b
  => Row.Cons l (Proxy2 a) rest s
  => Row.Cons l (Proxy2 b) rest t
  => SProxy l
  -> Lens (Smash s) (Smash t) a b
label l = lens \s -> runExists go (uncons l s) where
  go :: forall x y. Uncons a rest x y -> (a ⊗ Hom b (Smash t)) x
  go (Uncons a sf) = day (#) a (hom \b -> cons l identity b sf)

-- | Lower to a comonad identified by an optic.
-- |
-- | For example:
-- |
-- | ```purescript
-- | lowerOf first
-- |   :: forall g f
-- |    . Functor f
-- |   => Comonad g
-- |   => Day f g ~> f
-- |
-- | lowerOf (first <<< second)
-- |   :: forall f g h
-- |    . Functor g
-- |   => Comonad f
-- |   => Comonad h
-- |   => Day (Day f g) h ~> g
-- | ```
lowerOf
  :: forall s t a
   . Functor a
  => Comonad t
  => Optic (Split a) s t a Identity
  -> s ~> a
lowerOf o = runDay (\f x y -> f (extract x) <$> y) <<< runSplit (o (Split introDay1))

-- | Lift an action in the `Co` monad over the specified optic.
-- |
-- | For example:
-- |
-- | ```purescript
-- | liftCoOf first
-- |   :: forall g f
-- |    . Functor f
-- |   => Comonad g
-- |   => Co f ~> Co (Day f g)
-- |
-- | liftCoOf (first <<< second)
-- |   :: forall f g h
-- |    . Functor g
-- |   => Comonad f
-- |   => Comonad h
-- |   => Co g ~> Co (Day (Day f g) h)
-- | ```
liftCoOf
  :: forall s t a
   . Functor a
  => Comonad t
  => Optic (Split a) s t a Identity
  -> Co a ~> Co s
liftCoOf o a = co (runCo a <<< lowerOf o)

-- | Split a functor into a Day convolution.
-- | This is an implementation detail of the `lowerOf` function.
newtype Split r f (g :: Type -> Type) = Split (f ~> g ⊗ r)

runSplit :: forall f g r. Split r f g -> f ~> g ⊗ r
runSplit (Split n) = n

instance profunctorSplit :: Profunctor (Split r) where
  dimap f g (Split n) = Split (f >>> n >>> hoistDay1 g)

instance strongSplit :: Strong (Split r) where
  first (Split n) = Split (assoclDay <<< hoistDay2 symmDay <<< assocrDay <<< hoistDay1 n)
  second (Split n) = Split (assoclDay <<< hoistDay2 n)
