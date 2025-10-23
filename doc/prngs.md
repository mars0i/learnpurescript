
Purescript's Effect.Random wraps Javascript's Math.Random.

    https://pursuit.purescript.org/packages/purescript-random/6.0.0/docs/Effect.Random

Sebastiano Vigna says (https://vigna.di.unimi.it/#prngs):

    I designed a few pseudorandom number generators based on
    linear recurrences. In particular, xorshift128+ is now used
    in the JavaScript engines of Chrome, Node.js, Firefox, Safari
    and Microsoft Edge.

    Faster and better generators are described in the PRNG
    shootout page and in the papers linked therein.

So that seems like an OK PRNG, but Math.Random can't be seeded by the user.
