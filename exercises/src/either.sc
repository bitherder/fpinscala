import fpinscala.errorhandling.Either
import fpinscala.errorhandling.Left
import fpinscala.errorhandling.Right

Right(3).map(_ * 3)
Left("error").map((x: Int) => x * 3)

Right(3).flatMap(Right(_))
Left("Error").flatMap((x: Int) => Right(x))
Right(3).flatMap(x => Left("Got here"))

Right(3).map2(Right(4))((x, y) => x - y)
Left("bad one").map2(Right(4))((x: Int, y: Int) => x - y)
Right(3).map2(Left("bad two"))((x: Int, y: Int) => x - y)

Right(3).orElse(Right(4))
Left("too bad").orElse(Right(4))
Left("too bad").orElse(Left("worse"))

Either.traverse(List("1", "2", "3"))(i => Either.Try(i.toInt))
Either.traverse(List("1", "bob", "3"))(i => Either.Try(i.toInt))
Either.traverse(List("1", "bob", "jim"))(i => Either.Try(i.toInt))

Either.sequence(List(Right(1), Right(2), Right(3)))
Either.sequence(List(Right(1), Left("wrong"), Right(3)))
Either.sequence(List(Right(1), Left("wrong"), Left("wronger")))
Either.sequence(List(Right(1), Right(2), Left("wronger")))
