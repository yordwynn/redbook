package fpinscala.state

import org.scalatest.flatspec.AnyFlatSpec

class StateTest extends AnyFlatSpec {
    "RNG#nonNegativeInt" should "return non negative int" in {
        val (value, _) = RNG.nonNegativeInt(RNG.Simple(0))
        assert(value >= 0)
    }
}