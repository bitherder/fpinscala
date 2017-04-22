package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //  def take(n: Int): Stream[A] = (n, this) match {
  //    case (0, _) => empty
  //    case (_, Empty) => empty
  //    case (n, Cons(h, t)) => cons(h(), t().take(n - 1))
  //  }
  def take(n: Int): Stream[A] = unfold(n, this){
    {
      case (x, Cons(h, t)) if x > 0 => Some(h(), (x - 1, t()))
      case _ => None
    }
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = (n, this) match {
    case (0, xs) => xs
    case (_, Empty) => empty
    case (x, Cons(_, t)) => t().drop(x - 1)
  }

//  def takeWhile(p: A => Boolean): Stream[A] = this match {
//    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
//    case _ => empty
//  }

  // def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)
  def takeWhile(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) => if(p(h())) Some(h(), t()) else None
    case _ => None
  }

  def zipWith[B](other: Stream[B]): Stream[(A, B)] = unfold((this, other)){
    {
      case (Cons(th, tt), Cons(oh, ot)) => Some(((th(), oh()), (tt(), ot())))
      case _ => None
    }
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  // def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
  def map[B](f: A => B): Stream[B] = unfold(this){
    {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def append[B>:A](other: => Stream[B]) = foldRight(other)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]) = foldRight(empty[B])((a, b) => f(a) append b)

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (empty, t2()))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s)
      .takeWhile(_._2.isDefined) // { case (_, Some(_)) => true; case _ => false }.
      .forAll{ case (a, b) => a == b }

  def toList(): List[A] =  {
    def loop(xs: Stream[A], acc: List[A]): List[A] = xs match {
      case Empty => acc
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, List[A]()).reverse
  }

  def listOf(n: Int): List[A] = this.take(n).toList()

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a: A, s: S)) => cons(a, unfold(s)(f))
  }

  // val ones: Stream[Int] = Stream.cons(1, ones)
  val ones: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  // def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
//  def constant[A](a: A): Stream[A] = {
//    lazy val tail: Stream[A] = Cons(() => a, () => tail)
//    tail
//  }
  def constant[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

//  def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))
  def from(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

//  def fibs() = {
//    def loop(x: Int, y: Int): Stream[Int] = {
//      Stream.cons(x, loop(y, x + y))
//    }
//
//    loop(0, 1)
//  }

  def fibs(): Stream[Int] = unfold((0, 1))( { case (x, y) => Some((x, (y, x + y))) } )
}