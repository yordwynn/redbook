package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](as: Tree[A]): Int = {
    as match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }
  }

  def maximum(ints: Tree[Int]): Int = {
    ints match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  def depth[A](as: Tree[A]): Int = {
    as match {
      case Leaf(_) => 0
      case Branch(left, right) => depth(left).max(depth(right)) + 1
    }
  }

  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = {
    as match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A, B](as: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    as match {
      case Leaf(value) => f(value)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g)) 
    }
  }

  def sizeViaFold[A](as: Tree[A]): Int = {
    fold(as)(_ => 1)((x, y) => 1 + x + y)
  }

  def maximumViaFold(ints: Tree[Int]): Int = {
    fold(ints)(identity)((x, y) => x.max(y))
  }

  def depthViaFold[A](as: Tree[A]): Int = {
    fold(as)(_ => 0)((x, y) => x.max(y) + 1)
  }

  def mapViaFold[A, B](as: Tree[A])(f: A => B): Tree[B] = {
    fold(as)(a => Leaf(f(a)): Tree[B])((x, y) => Branch(x, y))
  }
}
