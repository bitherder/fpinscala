import fpinscala.errorhandling.Some
import fpinscala.errorhandling.None
import fpinscala.errorhandling.Option

Option.mean(List()).getOrElse(0)
Option.mean(List(1, 2, 3)).getOrElse(0)

val list123 = List(1.0, 2.0, 3.0)
val list1pmhalf = List(0.5, 1, 1.5)
Option.variance(Nil).getOrElse(-1)
Option.variance(list123).getOrElse(-1)
Option.variance(list123).map(_ * 2).getOrElse(-1)
Option.variance(list123).filter(_ > 0.5).getOrElse(-1)
Option.variance(list1pmhalf).filter(_ > 0.5).getOrElse(-1)

Option.map2(Some(2), Some(1))(_ - _)
Option.map2(None, Some(1))((x: Int, y) => x - y)
Option.map2(Some(2), None)((x, y: Int) => x - y)
Option.map2(None: Option[Int], None: Option[Int])(_ - _)

Option.sequence(List(Some(1), Some(2), Some(3)))
Option.sequence(List(None, Some(2), Some(3)))
Option.sequence(List(Some(1), None, Some(3)))
Option.sequence(List(Some(1), Some(2), None))

def Try[A](a: => A): Option[A] = {
  try Some (a)
  catch { case e: Exception => None }
}

Option.traverse(List("1", "2", "3"))(s => Try(s.toInt))
Option.traverse(List("x", "2", "3"))(s => Try(s.toInt))
Option.traverse(List("1", "2", ""))(s => Try(s.toInt))



