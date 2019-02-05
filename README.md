## Notes on categorical structures in Haskell.

These are a collection of some notes on category theory collected from various sources. Sometimes I learn something new about category theory this way.

### things I've learned recently (sorry if this is newbish!)

1) Language pragmas are important for extending GHC. So far I've used `PolyKinds`, `TypeOperators`, `MultiParamTypeClasses`, `RankNTypes`.

2) Yoneda's lemma could be rephrased as saying the right Kan extension of a functor along the identity is itself.

3) Kan extensions are pretty cool. Given an adjoint pair, we can write each functor as a Kan extension of the other. Precisely, if `F:C->D` and `G:D->C` are an adjoint pair (`F = left`) then `G` is the left Kan extension of `id:C->C` along `F`, and `F` is the right Kan extension of `id:D->D` along `G`,

`G = Lan F Identity`

and

`F = Ran G Identity`

This all conspires to leading to the **codensity monad**. In essence, adjoint pairs give rise to monads. But when we don't have an adjoint pair, but instead just a right Kan extension `Ran G Identity`, we can still form the codensity monad. This monad agrees with the monad of an adjunction if the right Kan extension is preserved by the left adjoint `F`.