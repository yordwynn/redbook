package fpinscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.datastructures.List.{tail, setHead, drop}

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

  "list" should "handle drop" in {
    assert(
      drop(Nil, 0) == Nil,
      "drop of zero elements from empty list is empty list"
    )
    assert(
      drop(Nil, 1) == Nil,
      "drop of one element from empty list is empty list"
    )
    assert(
      drop(Nil, 10) == Nil,
      "drop of many elements from empty list is empty list"
    )
    assert(
      drop(List(3), 0) == List(3),
      "drop of zero elements from single-element list is the list"
    )
    assert(
      drop(List(3), 1) == Nil,
      "drop of one element from single-element list is empty list"
    )
    assert(
      drop(List(3), 10) == Nil,
      "drop of many elements from single-element list is empty list"
    )
    assert(
      drop(List(1, 2, 3), 0) == List(1, 2, 3),
      "drop of zero elements from list is list"
    )
    assert(
      drop(List(1, 2, 3), 1) == List(2, 3),
      "drop of one elements from list is list without 1st element"
    )
    assert(
      drop(List(1, 2, 3), 2) == List(3),
      "drop of n elements from list is list without 1st n elements"
    )
    assert(
      drop(List(1, 2, 3), 10) == Nil,
      "drop of too many elements from list is empty list"
    )
  }
}
