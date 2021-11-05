package fpinscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.datastructures.List.tail

class ListTest extends AnyFlatSpec {
  "tail" should "handle arbitrary lists" in {
    assert(tail(Nil) == Nil)
    assert(tail(List(3)) == Nil)
    assert(tail(List(1, 2, 3)) == List(2, 3))
  }
}
