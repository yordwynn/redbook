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

  "RNG#intDouble" should "return differet int and double numbers" in {
    val ((intVal, doubleVal), _) = RNG.intDouble(RNG.Simple(0))
    assert(intVal.toDouble != doubleVal)
  }

  "RNG#doubleInt" should "return differet double and int numbers" in {
    val ((doubleVal, intVal), _) = RNG.doubleInt(RNG.Simple(0))
    assert(intVal.toDouble != doubleVal)
  }

  "RNG#double3" should "return three differet double numbers" in {
    val ((double1, double2, double3), _) = RNG.double3(RNG.Simple(0))
    assert(double1 != double2 && double1 != double3 && double3 != double2)
  }
}
