package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed =
        (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(
        newSeed
      ) // The next state, which is an `RNG` instance created from the new seed.
      val n =
        (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (
        n,
        nextRNG,
      ) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (value, state) = rng.nextInt
    (Math.abs(value), state)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (value, state) = rng.nextInt
    ((value.toDouble / Int.MaxValue).abs, state)
  }

  def doubleViaMap(rng: RNG): (Double, RNG) = {
    map(s => s.nextInt)(a => a.toDouble.abs / Int.MaxValue)(rng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intVal, st1) = rng.nextInt
    val (dblVal, st2) = double(st1)
    ((intVal, dblVal), st2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (dblVal, st1) = double(rng)
    val (intVal, st2) = st1.nextInt
    ((dblVal, intVal), st2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, s1) = double(rng)
    val (d2, s2) = double(s1)
    val (d3, s3) = double(s2)
    ((d1, d2, d3), s3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(n: Int, s: RNG, res: List[Int]): (List[Int], RNG) = {
      if (n > 0) {
        val (i, newS) = s.nextInt
        go(n - 1, newS, i +: res)
      } else {
        (res, s)
      }
    }

    go(count, rng, Nil)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng =>
      {
        val (a, rng1) = ra(rng)
        val (b, rng2) = rb(rng1)

        (f(a, b), rng2)
      }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))(
      (f, b) => map2(f, b)((fr, br) => fr +: br)
    )
  }

  def intsViaSequense(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill[Rand[Int]](count)(r => r.nextInt))(rng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, state) = f(rng)
    g(a)(state)
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2ViaFlatMap[A, B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C) = {
    flatMap(s1)(a => flatMap(s2)(b => unit(f(a, b))))
  }
}

case class State[S, +A](run: S => (A, S)) {
  def unit[B >: A](b: B): State[S, B] = State(s => (b, s))

  def map[B](f: A => B): State[S, B] =
    State(
      s => {
        val (a, newS) = run(s)
        (f(a), newS)
      }
    )

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => flatMap(a => sb.map(b => f(a, b))).run(s))
    

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, newS) = run(s)
        f(a).run(newS)
      }
    )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
