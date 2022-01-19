package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

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

trait Prop {
  def check: Boolean = ???

  def &&(p: Prop): Prop = AndProp(this, p)
}

object Prop {
  case class AndProp(left: Prop, right: Prop) extends Prop {
    override def check: Boolean = left.check && right.check
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object StrangeGen {
  object Gen {
    def unit[A](a: => A): Gen[A] = ???
  }

  trait Gen[A] {
    def map[A, B](f: A => B): Gen[B] = ???
    def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
  }
}

trait SGen[+A] {}

case class Gen[+A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = 
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = 
    size.flatMap(n => Gen.listOfN(n, this))
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

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = new Gen[List[A]](
    State(
      s => {
        RNG.sequence(List.fill(n)(g.sample.run))(s)
      }
    )
  )
}
