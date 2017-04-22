import fpinscala.laziness._

val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
val e = Stream.empty[Int]

s.toList()
e.toList()

s.take(5)
e.take(5)

s.take(5).toList()
e.take(5).toList()

s.drop(5).toList()
e.drop(5).toList()

s.takeWhile(_ < 5).toList()
e.takeWhile(_ < 5).toList()

s.forAll(_ < 100)
s.forAll(_ < 5)
s.forAll(_ < 11)
e.forAll(_ < 10)

s.headOption
e.headOption

e.listOf(10)

s.map(x => x*x).toList()
e.map(x => x*x).toList()

s.filter((x) => x % 2 == 0).toList()
e.filter((x) => x % 2 == 0).toList()

s.append(s).toList()
e.append(s).toList()
s.append(e).toList()

s.flatMap(x => Stream(x)).toList()
e.flatMap(x => Stream(x)).toList()

Stream.ones.listOf(10)

Stream.ones.exists(_ % 2 != 0)

Stream.from(1).listOf(10)

Stream.fibs().listOf(10)

Stream.fibs.zipWith(Stream.from(1)).listOf(10)
e.zipWith(Stream.from(1)).listOf(10)
Stream.fibs.zipWith(e).listOf(10)
s.zipWith(Stream.from(1)).listOf(20)
Stream.fibs.zipWith(s).listOf(20)
