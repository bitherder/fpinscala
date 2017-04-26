import fpinscala.state._

val rng = RNG.Simple(44)

val ints = RNG.ints(3)(rng)

RNG.nonNegativeInt(rng)

RNG.double(rng)

RNG.intDouble(rng)._1

List.fill(3)(1)

RNG.ints(5)(rng)._1

