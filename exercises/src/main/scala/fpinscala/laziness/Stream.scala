package fpinscala.laziness

import Stream._
import scala.annotation.tailrec

trait Stream[+A] {
  def foldRight[B](
    z: => B
  )(f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(
          h(),
          t().foldRight(z)(f),
        ) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)(
      (a, b) => p(a) || b
    ) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = {
    @tailrec
    def go(source: Stream[A], res: List[A]): List[A] = {
      source match {
        case Cons(h, t) => go(t(), h() +: res)
        case Empty => res
      }
    }

    go(this, List.empty).reverse
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  @tailrec
  final def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) cons(a, b) else Stream.empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption: Option[A] = {
    foldRight(Option.empty[A])((a, _) => Some(a))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
  }

  def append[B >: A](bs: => Stream[B]): Stream[B] = {
    foldRight(bs)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a).append(b))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (_, 0) => None
      case (Empty, _) => None
      case (Cons(h, t), i) => Some(h(), (t(), i - 1))
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, bs)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    }
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, bs)) {
      case (Empty, Empty) => None
      case (Empty, Cons(h, t)) => Some(((None, Some(h())), (Empty, t())))
      case (Cons(h, t), Empty) => Some(((Some(h()), None), (t(), Empty)))
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((Some(h1()), Some(h2())), (t1(), t2())))
    }
  }

  def zip[B](bs: Stream[B]): Stream[(A, B)] = {
    unfold((this, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(((h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    zipAll(s).foldRight(true)((a, b) => a match {
      case (Some(h1), Some(h2)) => h1 == h2 && b
      case (None, None) => true
      case (Some(_), None) => true
      case (None, Some(_)) => false
    })
  }

  def tails: Stream[Stream[A]] = {
    unfold(this){
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(Stream(empty))
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight((z, cons(z, empty)))((a, b) => {
      lazy val b1 = b
      val v = f(a, b._1)
      (v, cons(v, b._2))
    })._2
  }
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(x: Int, y: Int): Stream[Int] = cons(x, go(y, x + y))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty[A]
      case Some(a -> s) => cons(a, unfold(s)(f))
    }
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1)) {
      case (i, j) => Some((i, (j, i + j)))
    }
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(s => Some(s, s))
  }

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(s => Some(s, s))
  }
}
