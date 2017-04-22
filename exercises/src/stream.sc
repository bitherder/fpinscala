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

Stream.fibs.zipAll(Stream.from(1)).listOf(20)
e.zipAll(Stream.from(1)).listOf(10)
Stream.fibs.zipAll(e).listOf(10)
s.zipAll(Stream.from(1)).listOf(20)
Stream.fibs.zipAll(s).listOf(20)

e.startsWith(e)
e.startsWith(s)
s.startsWith(s)

e.startsWith(e)
e.startsWith(s)
s.startsWith(e)
s.startsWith(s)
Stream.from(1).startsWith(s)
s.startsWith(Stream.from(1).take(10))
s.startsWith(Stream.from(1).take(11))
Stream.from(1).startsWith(Stream.from(1).take(11))
Stream.from(1).startsWith(Stream(1, 2, 3, 4, 5, 6, 6))

s.tails.toList.map(_.toList)
s.scanRight(0)(_ + _).toList
s.scanRight(Stream.empty[Int])((a, b) => Stream.cons(a, b)).toList.map(_.toList)

