import fpinscala.laziness._

val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  s.toList()

s.take(5)

s.take(5).toList()

s.drop(5).toList()

s.takeWhile(_ < 5).toList()

s.forAll(_ < 100)

s.forAll(_ < 5)

s.forAll(_ < 11)

s.headOption

Stream.empty.headOption

s.map(x => x*x).toList()

s.filter((x) => x % 2 == 0).toList()

s.append(s).toList()

s.flatMap(x => Stream(x)).toList()

Stream.ones.listOf(10)

Stream.ones.exists(_ % 2 != 0)

Stream.constant(3).listOf(100)

Stream.from(3).listOf(10)

def plusPrev(s: Stream[Int]) = {
  def loop(n: Int, xs: Stream[Int]): Stream[Int] = xs match {
    case Cons(h, t) => Stream.cons(n + h(), loop(h(), t()))
  }

  loop(0, s)
}

plusPrev(Stream.from(1)).listOf(10)

Stream.fibs().listOf(10)

Stream.unfold((0, 1)){
  s => s match{
    case (x, y) => Some((x, (y, x + y)))
    case _ => None
  }
}.listOf(10)


