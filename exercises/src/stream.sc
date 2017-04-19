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



