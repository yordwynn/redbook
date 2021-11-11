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
}
