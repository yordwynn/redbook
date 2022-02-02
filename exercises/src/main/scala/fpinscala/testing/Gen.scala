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

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rnd) =>
      run(max, n, rnd) match {
        case Passed => p.run(max, n, rnd)
        case f: Falsified => f
        case Result.Proved => p.run(max, n, rnd)
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rnd) =>
      run(max, n, rnd) match {
        case Passed => Passed
        case Result.Proved => Result.Proved
        case f: Falsified => p.run(max, n, rnd)
      }
  }
}

object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    import Result._

    (max, n, rng) =>
      randomStream(gen)(rng)
        .zip(Stream.from(0)).take(n).map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
        }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map(
            p =>
              Prop {
                (max, _, rng) =>
                  p.run(max, casesPerSize, rng)
              }
          ).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e
      .getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis),
  ): Boolean = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")
      false
    case Passed =>
      println(s"+ OK, passed $testCases tests.")
      true
    case Result.Proved =>
      println(s"+ OK, proved property.")
      true
  }

  def check(p: => Boolean): Prop = Prop {
    (_, _, _) => if (p) Result.Proved else Falsified("()", 0)
  }

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25,
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop = Prop {
    (_, _, rnd) =>
      val (es, _) = S.sample.run(rnd)
      if (p(es).get()) Result.Proved else Falsified("()", 0)
  }
}

sealed trait Result {
  def isFalsified: Boolean
}

object Result {
  case object Passed extends Result { def isFalsified = false }

  case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result { def isFalsified = true }

  case object Proved extends Result { def isFalsified: Boolean = false }
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))

  def map[B](f: A => B): SGen[B] =
    SGen(n => forSize(n).map(f))
}

object SGen {
  def union[A](g1: SGen[A], g2: SGen[A]): SGen[A] =
    SGen(n => Gen.union(g1.forSize(n), g2.forSize(n)))
}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(a => f(a)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen(_ => this)

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.flatMap(a => g.sample.map(b => f(a, b))))
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

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val weight = g1._2 / (g1._2 + g2._2)
    Gen.double.flatMap(d => if (d < weight) g1._1 else g2._1)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n.max(1), g))

  def nestedPar[A](g: Gen[A], f: Gen[(A, A) => A]): Gen[Par[A]] = {
    def go(size: Int, res: Gen[Par[A]]): Gen[Par[A]] = {
      if (size == 0)
        res
      else
        weighted(
          res -> 0.75,
          go(size - 1, g.map(Par.unit(_))).flatMap(
            p1 =>
              res.flatMap(
                p2 => f.map(func => Par.fork(Par.map2(p1, p2)(func)))
              )
          ) -> 0.25,
        )
    }

    choose(0, 100).flatMap(n => go(n, g.map(Par.unit(_))))
  }

  lazy val pint2: Gen[Par[Int]] = choose(-100, 100)
    .listOfN(choose(0, 20)).map(
      l =>
        l.foldLeft(Par.unit(0))(
          (p, i) => Par.fork(Par.map2(p, Par.unit(i))(_ + _))
        )
    )
}

object Tests {
  val smallInt = Gen.choose(-10, 10)

  val maxProp = forAll(listOf1(smallInt)) {
    ns =>
      val max = ns.max
      !ns.exists(_ > max)
  }

  val sortedProp = forAll(listOf(smallInt)) {
    ns =>
      val sorted = ns.sorted

      if (ns.isEmpty || ns.size == 1)
        true
      else
        sorted.tail.zipWithIndex.forall {
          case (n, i) => n >= sorted(i)
        }
  }

  val p2 = checkPar {
    Par.equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2),
    )
  }

  val frk = {
    val p =
      Gen.nestedPar(Gen.choose(0, 10), Gen.unit((a: Int, b: Int) => a + b))
    forAllPar(p)(n => Par.equal(Par.fork(n), n))
  }
}
