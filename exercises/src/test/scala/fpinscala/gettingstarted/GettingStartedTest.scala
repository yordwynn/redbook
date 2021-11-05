package fpinscala.gettingstarted

import org.scalatest.flatspec.AnyFlatSpec

import fpinscala.gettingstarted.MyModule.fib
import fpinscala.gettingstarted.PolymorphicFunctions._

class GettingStartedTest extends AnyFlatSpec {
  "fib" should "handle positive integers" in {
    assert(fib(5) == 5)
  }

  "fib" should "handle zeroes" in {
    assert(fib(0) == 0)
  }

  "isSorted" should "handle non-empty lists" in {
    assert(isSorted[Int](Array(1, 2, 3), (x, y) => x < y))
  }

  "curry" should "curry :)" in {
    assert(curry[Int, Int, Int]((x, y) => x + y)(1)(2) == 3)
  }

  "uncurry" should "uncurry" in {
    assert(uncurry[Int, Int, Int](x => y => x * y)(1, 2) == 2)
  }

  "compuse" should "compuse" in {
    assert(compose[Int, Double, Double](x => x * 3, x => x + 1)(1) == 6)
  }
}
