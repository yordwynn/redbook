package fpinscala.datastructures

import scala.annotation.tailrec
import scala.collection.IndexedSeqView

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(
          xs
        ) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](
    as: List[A],
    z: B,
  )(f: (A, B) => B
  ): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => Cons(h, tail)
    }
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n > 0) {
      l match {
        case Nil =>
          Nil
        case Cons(head, tail) =>
          drop(tail, n - 1)
      }
    } else {
      l
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, b) => b + 1)
  }

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }
  }

  def sumViaFoldLeft(l: List[Int]): Int = 
    foldLeft(l, 0)(_ + _)

  def productViaFoldLeft(l: List[Double]): Double = 
    foldLeft(l, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = 
    foldLeft(l, 0)((b, _) => b + 1)

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))
  }

  def foldLeftViaFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((a, b) => f(b, a))
  }

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def appendViaFoldRight[A](as1: List[A], as2: List[A]): List[A] = {
    foldRightViaFoldLeft(as1, as2)((a, b) => Cons(a, b))
  }

  def concatenate[A](ass: List[List[A]]): List[A] = {
    foldRightViaFoldLeft(ass, Nil: List[A])((a, b) => appendViaFoldRight(a, b))
  }

  def addOne(ints: List[Int]): List[Int] = {
    foldRightViaFoldLeft(ints, Nil: List[Int])((a, b) => Cons(a + 1, b))
  }

  def doublesToStrings(doubles: List[Double]): List[String] = {
    foldRightViaFoldLeft(doubles, Nil: List[String])((a, b) => Cons(a.toString, b))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRightViaFoldLeft(l, Nil: List[B])((a, b) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(p: A => Boolean): List[A] = {
    foldRightViaFoldLeft(as, Nil: List[A])((a, b) => if (p(a)) Cons(a, b) else b)
  }
}
