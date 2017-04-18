sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def toString[A](xs: List[A]): String = xs match {
    case Nil => ""
    case Cons(y, Nil) => "" + y
    case Cons(h, t) => h + ", " + toString(t)
  }

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def foldLeft[A,B](l: List[A], z: B)(f:(B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldRight[A,B](l: List[A], z: B)(f:(B,A) => B): B =
    foldLeft(l, (b:B) => b)((g, a) => b => g(f(b,a)))(z)

  def append[A](xs: List[A], ys: List[A]): List[A] = foldRight(xs, ys)((l, a) => Cons(a, l))

  def concat[A](ll: List[List[A]]):List[A] = {
    foldRight(ll, List[A]())((acc, l) => append(l, acc))
  }

  def onePlus(xs: List[Int]): List[Int] =
    foldRight(xs, Nil:List[Int])((l, i) => Cons(i + 1, l))

  def dToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil:List[String])((l, d) => Cons(d.toString, l))

  def map[A, B](xs: List[A])(f: A => B): List[B] =
    foldRight(xs, Nil:List[B])((l, i) => Cons(f(i), l))

  def filter[A](xs: List[A])(f: A => Boolean): List[A] = {
    foldRight(xs, Nil:List[A])((b, a) => if(f(a)) Cons(a, b) else b)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((b, a) => append(f(a), b))
  }

  def filter2[A](xs: List[A])(f: A => Boolean): List[A] = {
    flatMap(xs)(a => if(f(a)) List(a) else Nil)
  }

  def sums(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(xh + yh, sums(xt, yt))
  }

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(f(xh, yh), zipWith(xt, yt)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(base: List[A], main: List[A], check: List[A]): Boolean = (base, main, check) match {
      case (_, _, Nil) => true
      case (Nil, _, _) => false
      case (_, Nil, _) => false
      case (_, Cons(mh, mt), Cons(ch, ct)) if mh == ch => loop(base, mt, ct)
      case (Cons(bh, bt), _, _) => loop(bt, bt, sub)
    }

    loop(sup, sup, sub)
  }
}

def listOfN(n:Int): List[Int] = {
  def go(i:Int): List[Int] =
    if(i > n) Nil
    else Cons(i, go(i + 1))
  go(1)
}

listOfN(6)

List.foldLeft(listOfN(6), "")(_ + _)
List.foldRight(listOfN(6), "")(_ + _)

List.toString(List.append(listOfN(3), listOfN(4)))

List.toString(List.concat(List(listOfN(2), listOfN(3), listOfN(4))))

List.toString(List.onePlus(listOfN(6)))

List.toString(List.dToString(List(1.2, 3.4, 5.6)))

List.toString(List.map(listOfN(6))(_ + 1))

List.toString(List.filter(listOfN(10))(_ % 2 == 0))
List.toString(List.filter2(listOfN(10))(_ % 2 == 0))

List.toString(List.flatMap(List(1,2,3))(i => List(i,i)))

List.toString(List.sums(List(1, 2, 3), List(10, 20, 30)))
List.toString(List.sums(List(1, 2), List(10, 20, 30)))
List.toString(List.sums(List(1, 2, 3), List(10, 20)))
List.toString(List.sums(Nil, Nil))

List.toString(List.zipWith(List(1, 2, 3), List(10, 20, 30))(_ + _))
List.toString(List.zipWith(List(1, 2), List(10, 20, 30))(_ + _))

List.hasSubsequence(List(1, 2, 3, 4), List(1))
List.hasSubsequence(List(1, 2, 3, 4), List(1, 2))
List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3))
List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4))

List.hasSubsequence(List(1, 2, 3, 4), List(2))
List.hasSubsequence(List(1, 2, 3, 4), List(2, 3))
List.hasSubsequence(List(1, 2, 3, 4), List(2, 3, 4))

List.hasSubsequence(List(1, 2, 3, 4), List(3))
List.hasSubsequence(List(1, 2, 3, 4), List(3, 4))

List.hasSubsequence(List(1, 2, 3, 4), List(4))

List.hasSubsequence(List(1, 2, 3, 4), Nil)

List.hasSubsequence(List(1, 2, 3, 4), List(5))
List.hasSubsequence(List(1, 2, 3, 4), List(1, 3))
