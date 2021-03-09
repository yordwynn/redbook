package fpinscala.laziness

import Stream._
import scala.annotation.tailrec
trait Stream[+A] {

  def foldRight[B](
    z: => B
  )(f: (A, => B) => B
  ): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
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
  def take(n: Int): Stream[A] = {
    this match {
      case _ if n == 0 => empty
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
      case Empty => empty
    }
  }

  def take2(n: Int): Stream[A] = {
    unfold((n, this))(s => s match {
      case (i, Cons(h, t)) if i > 0 => Some(h(), (i - 1, t()))
      case _ => None
    })
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

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  /**
    * takeWhile via unfold
    */
  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(this)(s => s match {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    })
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def headOption: Option[A] = {
    foldRight(Option.empty[A])((a, _) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => cons(f(a), b))
  }

  def map2[B](f: A => B): Stream[B] = {
    unfold(this)(s => s match {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    })
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, b) => if (p(a)) cons(a, b) else b)
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).foldRight(true)((a, b) => a match {
      case (Some(h1), Some(h2)) => b && h1 == h2
      case (Some(_), None) => true
      case (None, _) => false
    })
  }

  def toList: List[A] = {
    val buf = collection.mutable.ListBuffer[A]()

    @tailrec
    def go(s: Stream[A]): List[A] = {
      s match {
        case Cons(h, t) =>
          buf.append(h())
          go(t())
        case Empty => buf.toList
      }
    }

    go(this)
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, bs))(s => s match {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case (_, _) => None
    })
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2))(s => s match {
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty, t()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (_, _) => None
    })
  }

  def tails: Stream[Stream[A]] = {
    unfold(this)(s => s match {
      case Cons(_, t) => Some(s, t())
      case Empty => None
    }).append(Stream(empty))
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

  def ones2: Stream[Int] = unfold(1)(s => Some((s, s)))

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def constant2[A](a: A): Stream[A] = {
    unfold(a)(s => Some(s, s))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))
  }

  def fibs: Stream[Int] = {
    def nextFib(a: Int, b: Int): Stream[Int] =
      cons(a, nextFib(b, a + b))

    nextFib(0, 1)
  }

  def fibs2: Stream[Int] = {
    unfold[Int, (Int, Int)]((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some(head -> state) => cons(head, unfold(state)(f))
    }
  }
}
