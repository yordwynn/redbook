package fpinscala.errorhadling

import fpinscala.errorhandling._
import org.scalatest.flatspec.AnyFlatSpec

class EitherTest extends AnyFlatSpec {
  val right: Right[Int] = Right(0)
  val left: Either[Int, Int] = Left(0)

  "either" should "handle map" in {
    assert(right.map(_ + 10) == Right(10))
    assert(left.map(_ + 10) == Left(0))
  }

  "either" should "handle flatMap" in {
    assert(right.flatMap(x => Right(x + 5)) == Right(5))
    assert(left.flatMap(x => Right(x + 5)) == Left(0))
  }

  "either" should "handle orElse" in {
    assert(right.orElse(Left("Hello")) == Right(0))
    assert(left.orElse(Right("Hello")) == Right("Hello"))
  }

  "either" should "handle map2" in {
    assert(right.map2(Right(4))(_ + _ * 2) == Right(8))
    assert(right.map2(Left(10))(_ + 1 + _) == Left(10))
    assert(left.map2(Right(4))(_ + _ * 2) == Left(0))
    assert(left.map2(Left(4))(_ + _) == Left(0))
  }

//   "either" should "handle sequence" in {
//     val l1 = List(Right(1), Right(2), Right(3))
//     val l2 = List(Right(1), Left(2), Left(10), Right(3))
//     assert(Either.sequence(l1) == Right(List(1, 2, 3)))
//     assert(Either.sequence(l2) == Left(2))
//   }

//   "either" should "handle traverse" in {
//     val l1 = List(1, 2, 3, 0)
//     assert(
//       Either.traverse(l1)(
//         x =>
//           try { Right(x * 2) }
//           catch { case _: Throwable => Left(42) }
//       ) == Right(List(2, 4, 6, 0))
//     )
//     assert(
//       Either.traverse(l1)(
//         x =>
//           try { Right(0 / x) }
//           catch { case _: Throwable => Left(42) }
//       ) == Left(42)
//     )
//   }
}
