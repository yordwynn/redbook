package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
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

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    flatMap(s)(a => unit(f(a)))(rng)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (value, newRng) if value < 0 => (-1 * (value + 1), newRng)
      case res => res
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    nonNegativeInt(rng) match {
      case (intVal, state) => (intVal / (Int.MaxValue.toDouble + 1), state)
    }
  }

  def doubleViaMap: Rand[Double] = {
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intRnd, rng2) = rng.nextInt
    val (doubleRnd, rng3) = RNG.double(rng2)
    ((intRnd, doubleRnd), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (doubleRnd, rng2) = double(rng)
    val (intRnd, rng3) = rng2.nextInt
    ((doubleRnd, intRnd), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleRnd1, rng2) = double(rng)
    val (doubleRnd2, rng3) = double(rng2)
    val (doubleRnd3, rng4) = double(rng3)
    ((doubleRnd1, doubleRnd2, doubleRnd3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(cnt: Int, state: RNG, res: List[Int]): (List[Int], RNG) = {
      cnt match {
        case 0 => (res, state)
        case _ => {
          val (item, newState) = state.nextInt
          go(cnt - 1, newState, item +: res)
        }
      }
    }

    go(count, rng, List.empty)
  }

  def intsViaSequense(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))(rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(List.empty[A]))((acc, f) => map2(acc, f)((l, a) => a :: l))
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rnd => {
    val (a, rnd2) = f(rnd)
    g(a)(rnd2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod < 0) unit(mod) else nonNegativeLessThan(n)
    }
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldLeft(unit[S, List[A]](List.empty[A]))((b, sItem) => b.map2(sItem)((i, j) => j :: i))
  }

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
