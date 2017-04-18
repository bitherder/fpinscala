sealed trait Tree[+A] {
  def size: Int
}

case class Leaf[A](value: A) extends Tree[A] {
  def size: Int = 1
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def size: Int = 1 + left.size + right.size
}

object Tree {
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(p: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => p(fold(l)(f)(p), fold(r)(f)(p))
  }
}

Branch(Leaf(1), Leaf(2)).size
Tree.maximum(Branch(Leaf(4), Leaf(3)))
Tree.maximum(Branch(Branch(Leaf(5), Branch(Leaf(4), Leaf(2))), Leaf(3)))
Tree.depth(Branch(Branch(Leaf(5), Branch(Leaf(4), Leaf(2))), Leaf(3)))
Tree.depth(Leaf(4))
Tree.depth(Branch(Leaf(4), Leaf(3)))
Tree.map(Branch(Branch(Leaf(5), Branch(Leaf(4), Leaf(2))), Leaf(3)))(x => x * x)

Tree.fold(Leaf(4))(_ => 1)(1 + _ + _)
Tree.fold(Branch(Leaf(4), Leaf(3)))(_ => 1)(1 + _ + _)
Tree.fold(Branch(Leaf(4), Leaf(3)))(x => x)(_ + _)






