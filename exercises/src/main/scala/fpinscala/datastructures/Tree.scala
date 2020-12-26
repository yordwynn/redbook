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

    def maximum(intTree: Tree[Int]): Int = {
        intTree match {
            case Leaf(v) => v
            case Branch(left, right) => maximum(left).max(maximum(right))
        }
    }

    def depth[A](tree: Tree[A]): Int = {
        def go(tr: Tree[A], dpth: Int): Int = {
            tr match {
                case Leaf(_) => dpth
                case Branch(left, right) => go(left, dpth + 1).max(go(right, dpth + 1))
            }
        }

        go(tree, 0)
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
        tree match {
            case Leaf(value) => Leaf(f(value))
            case Branch(left, right) => Branch(map(left)(f), map(right)(f))
        }
    }
}