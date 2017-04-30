package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  // def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  //   rng => {
  //     val (a, rng2) = s(rng)
  //     (f(a), rng2)
  //   }
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (rawVal, newRng) = rng.nextInt
    val value = if(rawVal < 0) -(rawVal + 1) else rawVal
    (value, newRng)
  }

  def nonNegativeIntLessThan(limit: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
      val mod = i % limit
      if(i + (limit - 1) - mod >= 0) unit(mod) else nonNegativeIntLessThan(limit)
    }
  }

  //def double(rng: RNG): (Double, RNG) = {
  //  val (intVal, newRng) = nonNegativeInt(rng: RNG)
  //  (intVal.toDouble / (Int.MaxValue + 1), newRng)
  //}

  def double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

//  def intDouble(rng: RNG): ((Int,Double), RNG) = {
//    val (intVal, rng1) = rng.nextInt
//    val (doubleVal, rng2) = rng1.nextInt
//    ((intVal, doubleVal), rng2)
//  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def intDouble: Rand[(Int, Double)] = both(int, double)

  def doubleInt: Rand[(Double,Int)] = both(double, int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

//  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    def loop(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
//      if(c == 0)
//        (acc, r)
//      else {
//        val (intVal, nextRng) = r.nextInt
//        loop(c - 1, nextRng, intVal :: acc)
//      }
//    }
//
//    loop(count, rng, Nil)
//  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

//  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//    rnd => {
//      val (va, rnda) = ra(rnd)
//      val (vb, rndb) = rb(rnda)
//      (f(va, vb), rndb)
//    }
//  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){ a => flatMap(rb){ b => unit(f(a, b))}}

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))(map2(_, _)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (fa, frng) = f(rng)
      g(fa)(frng)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
