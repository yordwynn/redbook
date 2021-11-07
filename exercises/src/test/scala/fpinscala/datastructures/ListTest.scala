package fpinscala.datastructures

import org.scalatest.flatspec.AnyFlatSpec
import fpinscala.datastructures.List._

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
      "drop of zero elements from empty list is empty list",
    )
    assert(
      drop(Nil, 1) == Nil,
      "drop of one element from empty list is empty list",
    )
    assert(
      drop(Nil, 10) == Nil,
      "drop of many elements from empty list is empty list",
    )
    assert(
      drop(List(3), 0) == List(3),
      "drop of zero elements from single-element list is the list",
    )
    assert(
      drop(List(3), 1) == Nil,
      "drop of one element from single-element list is empty list",
    )
    assert(
      drop(List(3), 10) == Nil,
      "drop of many elements from single-element list is empty list",
    )
    assert(
      drop(List(1, 2, 3), 0) == List(1, 2, 3),
      "drop of zero elements from list is list",
    )
    assert(
      drop(List(1, 2, 3), 1) == List(2, 3),
      "drop of one elements from list is list without 1st element",
    )
    assert(
      drop(List(1, 2, 3), 2) == List(3),
      "drop of n elements from list is list without 1st n elements",
    )
    assert(
      drop(List(1, 2, 3), 10) == Nil,
      "drop of too many elements from list is empty list",
    )
  }

  "list" should "handle dropWhile" in {
    val positive = (x: Int) => x > 0
    assert(
      dropWhile(Nil, positive) == Nil,
      "dropWhile of empty list should be empty list",
    )
    assert(
      dropWhile(List(1), positive) == Nil,
      "dropWhile of list with single valid element should be empty list",
    )
    assert(
      dropWhile(List(1, 2, 3, 4), positive) == Nil,
      "dropWhile of list with only valid elements should be empty list",
    )
    assert(
      dropWhile(List(1, 2, -3, 4), positive) == List(-3, 4),
      "dropWhile of list with two leading valid elements should be list without leading elements",
    )
    assert(
      dropWhile(List(1, -2, -3, 4), positive) == List(-2, -3, 4),
      "dropWhile of list with one leading valid element should be list without leading element",
    )
    assert(
      dropWhile(List(-1, -2, -3, 4), positive) == List(-1, -2, -3, 4),
      "dropWhile of list with no leading valid elements should be same list",
    )
    assert(
      dropWhile(List(-1, -2, -3, -4), positive) == List(-1, -2, -3, -4),
      "dropWhile of list with no valid elements should be Nil",
    )
  }

  "list" should "handle init" in {
    assert(init(Nil) == Nil, "init of empty list should be empty list")
    assert(
      init(List(3)) == Nil,
      "init of single-element-list should be empty list",
    )
    assert(
      init(List(1, 2, 3)) == List(1, 2),
      "init of list should not have last element",
    )
  }

  "foldRight" should "return the same list" in {
    assert(
      foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3)
    )
  }

  "list" should "handle length via foldRight" in {
    assert(List.length(Nil) == 0, "length of empty list is zero")
    assert(List.length(List(1)) == 1, "length of single-element list is one")
    assert(List.length(List(1, 2, 3)) == 3, "length of n-element list is n")
  }

  "list" should "handle foldLeft" in {
    assert(
      foldLeft(List(1, 2, 3, 4, 5), 0)(_ + _) ==
      foldRight(List(1, 2, 3, 4, 5), 0)(_ + _),
      "foldLeft should compute the same sum value as foldRight",
    )

    assert(
      foldLeft(List(1, 2, 3, 4, 5), 1)(_ * _) ==
      foldRight(List(1, 2, 3, 4, 5), 1)(_ * _),
      "foldLeft should compute the same product value as foldRight",
    )

    assert(
      foldLeft(List("a", "b", "c"), "")(_ + _) ==
      foldRight(List("a", "b", "c"), "")(_ + _),
      "foldLeft should compute the same concatenation value as foldRight",
    )
  }

  "list" should "handle sum, product, and length via foldLeft" in {
    assert(sumViaFoldLeft(List(1, 2, 3)) == sum2(List(1, 2, 3)))
    assert(productViaFoldLeft(List(1, 2, 3)) == product2(List(1, 2, 3)))
    assert(lengthViaFoldLeft(List(1, 2, 3)) == List.length(List(1, 2, 3)))
  }

  "list" should "handle reverse" in {
    assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  "foldLeft via foldRight" should "reverse the list" in {
    assert(
      foldLeft(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(y, x)) == foldLeftViaFoldRight(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(y, x))
    )
  }

  "foldRight via foldLeft" should "not reverse the list" in {
    assert(
      foldRight(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(x, y)) == foldRightViaFoldLeft(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(x, y))
    )
  }
}
