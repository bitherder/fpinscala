import fpinscala.laziness._

val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

s.toList()

s.take(5)

s.take(5).toList()

s.drop(5).toList()

s.takeWhile(_ < 5).toList()



