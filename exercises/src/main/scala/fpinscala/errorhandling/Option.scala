package fpinscala.errorhandling

import scala.{
  Either => _,
  Option => _,
  Some => _,
  _,
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(get) => Some(f(get))
      case None => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(get) => get
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(_ => this).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    flatMap(v => if (f(v)) this else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int =
      throw new Exception(
        "fail!"
      ) // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception(
        "fail!"
      )): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).map(m => xs.map(x => math.pow(x - m, 2))).flatMap(mean)
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(oa => b.map(ob => f(oa, ob)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(List.empty[A]): Option[List[A]])(
      (opt, b) => map2(opt, b)((a, b) => a +: b)
    )
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Some(List.empty[B]): Option[List[B]])(
      (z, a) => z.flatMap(l => f(a).map(b => l :+ b))
    )
  }

  def sequenceViaTraverce[A](as: List[Option[A]]): Option[List[A]] = {
    traverse(as)(a => a)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }
}
