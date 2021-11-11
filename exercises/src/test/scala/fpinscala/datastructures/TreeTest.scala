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

  "depth" should "return the longest path" in {
    assert(depth(Branch(Branch(Leaf(1), Leaf(4)), Leaf(2))) == 2)
  }

  "map" should "modify each element of a tree" in {
    val expected = Branch(Branch(Leaf("1"), Leaf("4")), Branch(Leaf("2"), Leaf("3")))
    val mapped = map(Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(2), Leaf(3))))(_.toString)
    assert(expected == mapped)
  }
}
