package fpinscala.datastructures

import Tree._
import org.scalatest.flatspec.AnyFlatSpec

class TreeTest extends AnyFlatSpec {
    "size" should "return nubber of nodes" in {
        assert(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))) == 7)
    }
}