package fpinscala.datastructures

import fpinscala.datastructures.List.tail
import org.scalatest.flatspec.AnyFlatSpec

class ListTest extends AnyFlatSpec{
  "ex 3.1" should "return 3" in {
    assert(List.x == 3)
  }

  "tail" should "handle arbitrary lists" in {
    assert(tail(Nil) == Nil)
    assert(tail(List(3)) == Nil)
    assert(tail(List(1,2,3) ) == List(2,3))
  }
}
