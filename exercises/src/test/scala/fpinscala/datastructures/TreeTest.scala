package fpinscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.datastructures.Tree._

class TreeTest extends AnyFlatSpec {
  "size" should "return nubber of nodes" in {
    assert(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) == 7)
  }

  "maximum" should "return the maximum value of lists" in {
    assert(maximum(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(2), Leaf(3)))) == 4)
  }
}
