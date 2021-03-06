package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil
  extends List[
    Nothing
  ] // A `List` data constructor representing the empty list
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

  def product(ds: List[Double]): Double =
    ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x: Int = List(1, 2, 3, 4, 5) match {
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

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((b, a) => Cons(a, b))
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

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(
      _ * _
    ) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, as) => as
      case Nil => Nil
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, tail) => Cons(h, tail)
    }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    (l, n) match {
      case (_, 0) => l
      case (Nil, _) => Nil
      case (Cons(_, t), _) => drop(t, n - 1)
    }
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(l, (start: B) => start)((a, b) => b2 => b(f(b2, a)))(z)
  }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))

  def concatenate[A](ls: List[List[A]]): List[A] = {
    foldRight2(ls, Nil: List[A])(append)
  }

  def addOne(ints: List[Int]): List[Int] = {
    foldRight2(ints, Nil: List[Int])((a, acc) => Cons(a + 1, acc))
  }

  def doubleToString(doubles: List[Double]): List[String] = {
    foldRight2(doubles, Nil: List[String])((a, acc) => Cons(a.toString, acc))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    foldRight2(l, Nil: List[B])((a, acc) => Cons(f(a), acc))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    val buf = collection.mutable.ListBuffer[A]()
    @tailrec
    def go(l: List[A]): Unit = {
      l match {
        case Nil => ()
        case Cons(h, t) if f(h) =>
          buf += h
          go(t)
        case Cons(_, t) => go(t)
      }
    }
    go(as)
    List(buf.toList: _*)
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    foldRight2(l, Nil: List[B])((a, acc) => append(f(a), acc))
  }

  def filterViaFlatmap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def sumTwoLists(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, sumTwoLists(t1, t2))
    }
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    (as, bs) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def go(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => go(t1, t2)
        case (Cons(_, t), _) => go(t, sub)
      }
    }
    go(sup, sub)
  }
}
