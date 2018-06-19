module Data.Smash
  ( Smash
  , empty
  , singleton
  , cons
  , uncons
  , lower
  , smash
  , cosmash
  , cosmash_
  , Uncons(..)
  , class Smashed
  , smashRL
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
import Prim.Row as Row
import Record (delete, get, insert)
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Proxy (Proxy2)
import Type.Row (class RowToList, Cons, Nil, RLProxy(RLProxy))
import Unsafe.Coerce (unsafeCoerce)

-- | The result of extracting a single interpreter from a `Smash` product.
data Uncons f r a x = Uncons (f x) (Smash r (x -> a))

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

-- | Construct a value of type `Smash (l :: Proxy2 f)` by lifting a value
-- | of type `f a`.
singleton
  :: forall l r f a
   . IsSymbol l
  => Row.Cons l (Proxy2 f) () r
  => SProxy l
  -> f a
  -> Smash r a
singleton l fa = cons l const fa (empty unit)

-- | Add an interpreter of type `f a` to form a larger `Smash` product of
-- | interpreters.
cons
  :: forall l f r1 r2 a b c
   . IsSymbol l
  => Row.Cons l (Proxy2 f) r1 r2
  => SProxy l
  -> (a -> b -> c)
  -> f a
  -> Smash r1 b
  -> Smash r2 c
cons l f fa s = unsafeCoerce
  { parts: unsafeSet (reflectSymbol l) fa (unsafeCoerce s).parts
  , get: \r -> f (unsafeGet (reflectSymbol l) r) ((unsafeCoerce s).get r)
  }

-- | Smash together a record of interpreters to get an interpreters which
-- | returns records.
smash
  :: forall interpreters results proxies rl
   . RowToList interpreters rl
  => Smashed rl interpreters proxies results
  => Record interpreters
  -> Smash proxies (Record results)
smash = smashRL (RLProxy :: RLProxy rl)

-- | Project out the interpreter at the specified label, ignoring the future
-- | state of the other interpreters.
lower
  :: forall l f r rl rest a
   . IsSymbol l
  => Functor f
  => Row.Cons l (Proxy2 f) rest r
  => RowToList rest rl
  => ComonadSmash rl rest
  => SProxy l
  -> Smash r a
  -> f a
lower l s = runExists (\(Uncons here rest) -> extract rest <$> here) (uncons l s)

uncons
  :: forall l f r rest a
   . IsSymbol l
  => Row.Cons l (Proxy2 f) rest r
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
  => Row.Cons l (Proxy2 f) rest r
  => Functor f
  => RowToList rest rl
  => ComonadSmash rl rest
  => SProxy l
  -> (forall x. f (a -> x) -> x)
  -> Co (Smash r) a
cosmash l f = co (f <<< lower l)

-- | A simpler variant of `cosmash` for when you don't care about the result.
cosmash_
  :: forall l f r rest rl
   . IsSymbol l
  => Row.Cons l (Proxy2 f) rest r
  => Functor f
  => RowToList rest rl
  => ComonadSmash rl rest
  => SProxy l
  -> (forall x. f x -> x)
  -> Co (Smash r) Unit
cosmash_ l f = cosmash l \ff -> f ((_ $ unit) <$> ff)

class Smashed rl interpreters proxies results | rl -> interpreters proxies results where
  smashRL :: RLProxy rl -> Record interpreters -> Smash proxies (Record results)

instance smashedNil :: Smashed Nil () () () where
  smashRL _ _ = empty {}

instance smashedCons
  :: ( IsSymbol l
     , Row.Lacks l results_
     , Row.Lacks l interpreters_
     , Row.Lacks l proxies_
     , Row.Cons l (f a) interpreters_ interpreters
     , Row.Cons l (Proxy2 f) proxies_ proxies
     , Row.Cons l a results_ results
     , Smashed rl interpreters_ proxies_ results_
     )
  => Smashed (Cons l (f a) rl) interpreters proxies results where
  smashRL _ interpreters =
      cons l
           (insert l :: a -> Record results_ -> Record results)
           (get l interpreters)
           (smashRL (RLProxy :: RLProxy rl) (delete l interpreters))
    where
      l = SProxy :: SProxy l

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
     , Row.Cons l (Proxy2 f) r1 r
     , ExtendSmash rl r1
     )
  => ExtendSmash (Cons l (Proxy2 f) rl) r where
  duplicateSmashRL _ s = runExists go (uncons l s) where
    l :: SProxy l
    l = SProxy

    go :: forall a x. Uncons f r1 a x -> Smash r (Smash r a)
    go (Uncons part rest) =
      cons l
            (cons l (flip identity))
            (duplicate part)
            (duplicateSmashRL (RLProxy :: RLProxy rl) rest)

instance comonadSmashCons
  :: ( Comonad f
     , IsSymbol l
     , Row.Cons l (Proxy2 f) r1 r
     , ComonadSmash rl r1
     )
  => ComonadSmash (Cons l (Proxy2 f) rl) r where
  extractSmashRL _ s = runExists go (uncons l s) where
    l :: SProxy l
    l = SProxy

    go :: forall a x. Uncons f r1 a x -> a
    go (Uncons part rest) =
      extractSmashRL (RLProxy :: RLProxy rl) rest (extract part)
