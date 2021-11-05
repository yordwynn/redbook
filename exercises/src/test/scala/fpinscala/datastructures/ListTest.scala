package fpinscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.datastructures.List.{tail, setHead}

class ListTest extends AnyFlatSpec {
  "tail" should "handle arbitrary lists" in {
    assert(tail(Nil) == Nil)
    assert(tail(List(3)) == Nil)
    assert(tail(List(1, 2, 3)) == List(2, 3))
  }

  "list" should "handle set head" in {
    assert(setHead(Nil, 1) == Nil)
    assert(setHead(List(2), 1) == List(1))
    assert(setHead(List(3, 2), 1) == List(1, 2))
  }
}
