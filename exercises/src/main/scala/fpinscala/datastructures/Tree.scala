package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + l + r)

  def maximum(intTree: Tree[Int]): Int = {
    intTree match {
      case Leaf(v) => v
      case Branch(left, right) => maximum(left) max maximum(right)
    }
  }

  def maximumViaFold(intTree: Tree[Int]): Int = fold(intTree)(a => a)((l, r) => l max r)

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left) max depth(right)
    }
  }

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((l, r) => 1 + l max r)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
}
