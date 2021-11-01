import org.scalatest.flatspec.AnyFlatSpec

import fpinscala.gettingstarted.MyModule.fib

class GettingStartedTest extends AnyFlatSpec {
  "fib" should "handle positive integers" in {
    assert(fib(5) == 5)
  }

  "fib" should "handle zeroes" in {
    assert(fib(0) == 0)
  }
}
