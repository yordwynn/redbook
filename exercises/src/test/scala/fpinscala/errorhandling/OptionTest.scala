package fpinscala.errorhandling

import fpinscala.errorhandling._
import org.scalatest.flatspec.AnyFlatSpec

class OptionTest extends AnyFlatSpec {
  "option" should "handle map" in {
    assert(Some(1).map(_ * 2) == Some(2))
    assert((None: Option[Int]).map(_ * 2) == None)
  }

  "option" should "handle flatMap" in {
    assert(Some(1).flatMap(a => Some(a * 2)) == Some(2))
    assert((None: Option[Int]).flatMap(a => Some(a * 2)) == None)
  }

  "option" should "handle getOrElse" in {
    assert(Some(32).getOrElse(0) == 32)
    assert(None.getOrElse(42) == 42)
  }

  "option" should "handle orElse" in {
    assert(Some(42).orElse(None) == Some(42))
    assert(Some(42).orElse(Some(10)) == Some(42))
    assert(None.orElse(None) == None)
    assert(None.orElse(Some(10)) == Some(10))
  }

  "option" should "handle filter" in {
    assert(Some(20).filter(a => a % 2 == 0) == Some(20))
    assert(Some(21).filter(a => a % 2 == 0) == None)
    assert((None: Option[Int]).filter(a => a % 2 == 0) == None)
  }

  "option" should "handle variance" in {
    assert(Option.variance(Seq(1, 2, 3, 4)) == Some(1.25))
  }

//   "option" should "handle map2" in {
//     assert(Option.map2(Some(1), Some(2))(_ + _) == Some(3))
//     assert(Option.map2(None: Option[Int], Some(2))(_ + _) == None)
//     assert(Option.map2(Some(1), None: Option[Int])(_ + _) == None)
//     assert(Option.map2(Some(1), None: Option[Int])(_ + _) == None)
//     assert(Option.map2(None: Option[Int], None: Option[Int])(_ + _) == None)
//   }

//   "option" should "handle sequencce" in {
//     assert(Option.sequence(List(Some(1), None, Some(2))) == None)
//     assert(Option.sequence(List(Some(1), Some(2))) == Some(List(1, 2)))
//   }

//   "option" should "handle traverse" in {
//     assert(Option.traverse(List(0, 1, 2))(x => Option.Try(x + 1)) == Some(List(1, 2, 3)))
//     assert(Option.traverse(List(0, 1, 2))(x => Option.Try(1 / x)) == None)
//   }
}
