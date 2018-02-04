## purescript-smash

Extensible coeffects using comonads and Day convolution.

- [Example](test/Main.purs)
- [Module Documentation](generated-docs/Data/Smash.md)

### Getting Started

```
bower update
pulp build
pulp test
```

### Introduction

Given a monoidal category `(𝒞, ⊗, I)`, we can combine any non-zero number of objects in `𝒞` using the repeated application of the tensor product `⊗`. We can extend this to combinations of zero objects using unit object `I`.

Given a _row_ of objects in the category `𝒞`, we can combine the objects in the labels, and (in most cases) use the labels as "pointers" to the original objects inside the combination.

We already have several examples of this:

- Records are n-ary tuples, and the labels can be used to construct polymorphic _lenses_ which identify the type of each label as part of the product type.
- Variants (e.g. [`purescript-variant`](https://github.com/natefaubion/purescript-variant)) are n-ary sums, and the labels can be used to construct polymorphic _prisms_ which identify the type of each label as a summand.
- Higher-order variants (e.g. as used in [`purescript-run`](https://github.com/natefaubion/purescript-run)) are n-ary functor coproducts. We could use the labels to construct "functor coproduct optics".

This library constructs a data type `Smash` in the same way out of the symmetric monoidal category `(★ → ★, Day, Identity)` of functors and Day convolution.

We can use this data type to model extensible _coeffects_, in much the same way that `purescript-run` uses higher-order variants to model extensible effects. First, note that Day convolution preserves comonadic structure, so `Smash r` is a comonad whenever the functors appearing in the row `r` are themselves comonads. So we can construct an interpreter for a combination of coeffects by combining the appropriate comonads.

Next, we can _construct_ a monad of actions from our comonad, using Edward Kmett's monads-from-comonads constructor `Co`.

By starting with a comonadic _interpreter_ instead of a monadic language, we can freely compose any coeffects, since unlike monads, comonads do compose nicely using Day convolution. However, this is not a full replacement for monad transformers and the MTL, since we cannot construct all monads in this way. There is no way to model IO, for example, or even exceptions in the style of `Either`.

Just like we can derive lenses and prisms from the labels in a record or variant type, we can derive optics which focus on individual comonads inside our Day convolution of comonads. The `Data.Smash.Lens` module implements this idea.
