module Data.Smash
  ( FProxy(..)
  , Smash
  , empty
  , singleton
  , smash
  , lowerSmash
  , Uncons(..)
  , uncons
  , cosmash
  , cosmash_
  , class ExtendSmash
  , duplicateSmashRL
  , class ComonadSmash
  , extractSmashRL
  ) where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, duplicate)
import Data.Exists (Exists, mkExists, runExists)
import Data.Functor.Pairing.Co (Co, co)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Record.Unsafe (unsafeGet, unsafeSet)
import Type.Row (class RowToList, Cons, Nil, RLProxy(RLProxy), kind RowList)
import Unsafe.Coerce (unsafeCoerce)

-- | Smash a bunch of functors together with Day convolution
-- |
-- | The representation is abstract but can be understood in terms of
-- | the defintion of `Day`:
-- |
-- | ```purescript
-- | Day f g a ~ ∃x y. (f x, g y, x -> y -> a)
-- | ```
-- |
-- | So for a smash product of several functors, we need a supply xⱼ of
-- | existential variables, and then we define
-- |
-- | ```purescript
-- | Smash f a = ∃x1 ... xⱼ. (Πⱼ fⱼ xⱼ,, Πⱼ xⱼ -> a)
-- | ```
-- | which we represent using a record.
data Smash (r :: # Type) (a :: Type)

-- | A value-level representation of a functor, so that we can use
-- | some mono-kinded compiler-provided machinery like `RowCons`.
data FProxy (f :: Type -> Type) = FProxy

instance functorSmash :: Functor (Smash r) where
  map f s = unsafeCoerce
    { parts: (unsafeCoerce s).parts
    , get: f <<< (unsafeCoerce s).get
    }

instance extendSmash :: (RowToList r rl, ExtendSmash rl r) => Extend (Smash r) where
  extend f = map f <<< duplicateSmashRL (RLProxy :: RLProxy rl)

instance comonadSmash :: (RowToList r rl, ComonadSmash rl r) => Comonad (Smash r) where
  extract = extractSmashRL (RLProxy :: RLProxy rl)

-- | Construct a value of type `Smash ()` by lifting a value of type `a`.
empty :: forall a. a -> Smash () a
empty a = unsafeCoerce
  { parts: {}
  , get: \_ -> a
  }

-- | Construct a value of type `Smash (l :: FProxy f)` by lifting a value
-- | of type `f a`.
singleton
  :: forall l r f a
   . IsSymbol l
  => RowCons l (FProxy f) () r
  => SProxy l
  -> f a
  -> Smash r a
singleton l fa = smash l const fa (empty unit)

-- | Add an interpreter of type `f a` to form a larger `Smash` product of
-- | interpreters.
smash
  :: forall l f r1 r2 a b c
   . IsSymbol l
  => RowCons l (FProxy f) r1 r2
  => SProxy l
  -> (a -> b -> c)
  -> f a
  -> Smash r1 b
  -> Smash r2 c
smash l f fa s = unsafeCoerce
  { parts: unsafeSet (reflectSymbol l) fa (unsafeCoerce s).parts
  , get: \r -> f (unsafeGet (reflectSymbol l) r) ((unsafeCoerce s).get r)
  }

-- | Project out the interpreter at the specified label, ignoring the future
-- | state of the other interpreters.
lowerSmash
  :: forall l f r rl rest a
   . IsSymbol l
  => Functor f
  => RowCons l (FProxy f) rest r
  => RowToList rest rl
  => ComonadSmash rl rest
  => SProxy l
  -> Smash r a
  -> f a
lowerSmash l s = runExists (\(Uncons here rest) -> extract rest <$> here) (uncons l s)

data Uncons f r a x = Uncons (f x) (Smash r (x -> a))

uncons
  :: forall l f r rest a
   . IsSymbol l
  => RowCons l (FProxy f) rest r
  => SProxy l
  -> Smash r a
  -> Exists (Uncons f rest a)
uncons l s = mkExists (Uncons here rest) where
  here :: f Unit -- j/k
  here = unsafeGet (reflectSymbol l) (unsafeCoerce s).parts

  rest :: Smash rest (Unit -> a) -- j/k
  rest = unsafeCoerce
    { parts: (unsafeCoerce s).parts
    , get: \r x -> (unsafeCoerce s).get (unsafeSet (reflectSymbol l) x r)
    }

-- | A helper function for constructing actions in a `Co` monad.
cosmash
  :: forall l f r rest rl a
   . IsSymbol l
  => RowCons l (FProxy f) rest r
  => Functor f
  => RowToList rest rl
  => ComonadSmash rl rest
  => SProxy l
  -> (forall x. f (a -> x) -> x)
  -> Co (Smash r) a
cosmash l f = co (f <<< lowerSmash l)

-- | A simpler variant of `cosmash` for when you don't care about the result.
cosmash_
  :: forall l f r rest rl
   . IsSymbol l
  => RowCons l (FProxy f) rest r
  => Functor f
  => RowToList rest rl
  => ComonadSmash rl rest
  => SProxy l
  -> (forall x. f x -> x)
  -> Co (Smash r) Unit
cosmash_ l f = cosmash l \ff -> f ((_ $ unit) <$> ff)

class ExtendSmash rl r | rl -> r where
  duplicateSmashRL :: forall a. RLProxy rl -> Smash r a -> Smash r (Smash r a)

class ExtendSmash rl r <= ComonadSmash rl r | rl -> r where
  extractSmashRL :: forall a. RLProxy rl -> Smash r a -> a

instance extendSmashNil :: ExtendSmash Nil () where
  duplicateSmashRL _ s = empty s

instance comonadSmashNil :: ComonadSmash Nil () where
  extractSmashRL _ s = (unsafeCoerce s).get {}

instance extendSmashCons
  :: ( Extend f
     , IsSymbol l
     , RowCons l (FProxy f) r1 r
     , ExtendSmash rl r1
     )
  => ExtendSmash (Cons l (FProxy f) rl) r where
  duplicateSmashRL _ s = runExists go (uncons l s) where
    l :: SProxy l
    l = SProxy

    go :: forall a x. Uncons f r1 a x -> Smash r (Smash r a)
    go (Uncons part rest) =
      smash l
            (smash l (flip id))
            (duplicate part)
            (duplicateSmashRL (RLProxy :: RLProxy rl) rest)

instance comonadSmashCons
  :: ( Comonad f
     , IsSymbol l
     , RowCons l (FProxy f) r1 r
     , ComonadSmash rl r1
     )
  => ComonadSmash (Cons l (FProxy f) rl) r where
  extractSmashRL _ s = runExists go (uncons l s) where
    l :: SProxy l
    l = SProxy

    go :: forall a x. Uncons f r1 a x -> a
    go (Uncons part rest) =
      extractSmashRL (RLProxy :: RLProxy rl) rest (extract part)
