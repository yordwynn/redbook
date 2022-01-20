package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}
import fpinscala.testing.Result.Passed
import fpinscala.testing.Result.Falsified

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
 */

/** Ex 8.1. Sum has such properties as commutativity, distributivity, and
  * associativity. So, we shoul check these properties. What is the sum of an
  * empy list.
  */

/** Ex 8.2. The result of a maximum function has to be greater or equal to any
  * element of the list. If all elements of a list have the same value the
  * maximum has to be equal to any element of the list (we can chek the first
  * and the last elements only). If list consists of the onlt element the
  * maximum has to be equal to the element. What is the maximum of an empty
  * list? SHould it be None?
  */

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (n, rnd) => run(n, rnd) match {
      case Passed => p.run(n, rnd)
      case f: Falsified => f
    }
  }

  def ||(p: Prop): Prop = Prop {
    (n, rnd) => run(n, rnd) match {
      case Passed => Passed
      case f: Falsified => p.run(n, rnd)
    }
  }
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    import Result._

    (n, rng) =>
      randomStream(gen)(rng)
        .zip(Stream.from(0)).take(n).map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e
      .getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
}

sealed trait Result {
  def isFalsified: Boolean
}

object Result {
  case object Passed extends Result { def isFalsified = false }

  case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result { def isFalsified = true }
}

case class SGen[+A](forSize: Int => Gen[A])

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val weight = g1._2 / (g1._2 + g2._2)
    Gen.double.flatMap(d => if (d < weight) g1._1 else g2._1)
  }

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    new Gen[Int](
      State(
        s => {
          RNG.map(rnd => rnd.nextInt)(i => i % (stopExclusive - start) + start)(
            s
          )
        }
      )
    ) {}

  def unit[A](a: => A): Gen[A] = new Gen[A](State(s => RNG.unit(a)(s)))

  def boolean: Gen[Boolean] = new Gen[Boolean](
    State(s => RNG.map(rnd => rnd.nextInt)(i => i % 2 == 0)(s))
  )

  def double: Gen[Double] = new Gen[Double](
    State(RNG.double)
  )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = new Gen[List[A]](
    State(
      s => {
        RNG.sequence(List.fill(n)(g.sample.run))(s)
      }
    )
  )
}
