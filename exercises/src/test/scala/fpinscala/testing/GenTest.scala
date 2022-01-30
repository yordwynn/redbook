package fpinscala.testing

import org.scalatest.flatspec.AnyFlatSpec

class GenTest extends AnyFlatSpec {
  "Max prop" should "pass" in {
    assert(Prop.run(Tests.maxProp) == true)
  }
}
