package fpinscala.laziness

import org.scalatest.flatspec.AnyFlatSpec

class StreamTest extends AnyFlatSpec {
  "Stream" should "handle toList" in {
    val stream = Stream.apply(1, 2, 3, 4)
    assert(stream.toList == List(1, 2, 3, 4))
  }

  "Stream" should "handle take N elements" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    assert(stream.take(3).toList == List(1, 2, 3))
  }

  "Stream" should "handle drop N elements" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    assert(stream.drop(3).toList == List(4, 5))
  }

  "Stream" should "handle takeWhile" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    assert(stream.takeWhile(a => a != 3).toList == List(1, 2))
  }

  "Stream" should "handle forAll" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    assert(stream.forAll(a => a < 10) == true)
    assert(stream.forAll(a => a != 3) == false)
  }

  "Stream" should "handle takeWhile via foldRight" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    assert(stream.takeWhile2(a => a != 3).toList == List(1, 2))
  }

  "Stream" should "handle headOption via foldRight" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    val emptyStream = Stream.empty
    assert(stream.headOption == Some(1))
    assert(emptyStream.headOption == None)
  }

  "Stream" should "handle map via foldRight" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    val emptyStream = Stream.empty[Int]
    assert(stream.map(_ + 1).toList == List(2, 3, 4, 5, 6))
    assert(emptyStream.map(_ + 1).toList == List.empty)
  }

  "Stream" should "handle filter via foldRight" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    val emptyStream = Stream.empty[Int]
    assert(stream.filter(_ % 2 == 0).toList == List(2, 4))
    assert(emptyStream.filter(_ % 2 == 0).toList == List.empty)
  }

  "Stream" should "handle append via foldRight" in {
    val stream = Stream.apply(1, 2, 3, 4, 5)
    val emptyStream = Stream.empty[Int]
    assert(stream.append(Stream.apply(6)).toList == List(1, 2, 3, 4, 5, 6))
    assert(emptyStream.append(Stream.apply(1)).toList == List(1))
  }

  "Stream" should "handle flatMap via foldRight" in {
    val stream = Stream.apply(1, 2, 3)
    val emptyStream = Stream.empty[Int]
    assert(stream.flatMap(x => Stream.apply(x, x)).toList == List(1, 1, 2, 2, 3, 3))
    assert(emptyStream.flatMap(x => Stream.apply(x, x)).toList == List.empty)
  }

  "Stream" should "handle constant function" in {
    assert(Stream.constant(3).take(2).toList == List(3, 3))
  }

  "Stream" should "handle constant via unfold" in {
    assert(Stream.constant2(3).take(2).toList == List(3, 3))
  }

  "Stream" should "handle from function" in {
    assert(Stream.from(3).take(2).toList == List(3, 4))
  }

  "Stream" should "handle from via unfold" in {
    assert(Stream.from2(3).take(2).toList == List(3, 4))
  }

  "Stream" should "handle fibs function" in {
    assert(Stream.fibs.take(6).toList == List(0, 1, 1, 2, 3, 5))
  }

  "Stream" should "handle fibs via unfold" in {
    assert(Stream.fibs2.take(6).toList == List(0, 1, 1, 2, 3, 5))
  }

  "Stream" should "handle unfold" in {
    val infiniteAcual = Stream.unfold(1)(s => Some(s -> s)).take(3).toList
    val infiniteExpected = List(1, 1, 1)
    assert(infiniteExpected == infiniteAcual)

    val finiteActual = Stream.unfold(0)(s => if (s < 5) Some((s, s + 1)) else None).toList
    val finiteExpected = List(0, 1, 2, 3, 4)
    assert(finiteExpected == finiteActual)
  }

  "Stream" should "handle ones via unfold" in {
    assert(Stream.ones2.take(2).toList == List(1, 1))
  }
}
