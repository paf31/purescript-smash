## purescript-smash

Extensible coeffects using comonads and Day convolution.

- [Example](tests/Main.purs)
- [Module Documentation](generated-docs/Data/Smash.md)

### Introduction

One way to create a system for extensible algebraic effects is to use a free monad
over a coproduct of functors describing the effects we care about. The [`purescript-run` library](https://github.com/natefaubion/purescript-run) by Nathan Faubion takes this one step further and uses rows to cleverly describe the functors we are working with. Row polymorphism does the right thing when it comes to composing programs with different effects.

This library uses a related but slightly different approach, nicking the row polymorphism trick from `purescript-run` and applying it to a different type of functor composition - Day convolution.

First, instead of starting with actions and then defining their interpreters, let's start with the interpreters, and construct the actions from them. Monads don't compose nicely, hence the trick using coproducts of the _underlying_ functors described above. However, comonads _do_ compose in a nice, interesting way, using Day convolution. So the trick will be to describe our interpreter using a Day convolution of several comonads, and then to _construct_ the monad for our actions from that comonad (using Edward Kmett's monads-from-comonads constructor `Co`).

(Note that the Day convolution of cofree comonads is also a cofree comonad, and the monad constructed from a cofree comonad is a free monad, so we don't lose any expressiveness).

Seen another way, this approach lets us use the same techniques as the free-monad-of-coproducts approach, but avoiding some of the overhead of `Free` in certain cases, by using more efficient interpreters, quotienting by certain program equivalences.

### Building

This project uses `psc-package`. You can try it out by cloning this repo and running
`pulp build` or `psc-package build`.
