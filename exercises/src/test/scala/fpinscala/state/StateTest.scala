package fpinscala.state

import org.scalatest.flatspec.AnyFlatSpec

class StateTest extends AnyFlatSpec {
  "RNG#nonNegativeInt" should "return non negative int" in {
    val (value, _) = RNG.nonNegativeInt(RNG.Simple(0))
    assert(value >= 0)
  }

  "RNG#double" should "return double from the interval (0, 1)" in {
    val (value, _) = RNG.double(RNG.Simple(0))
    assert(0 < value && value < 1)
  }
}
