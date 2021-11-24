package fpinscala.laziness

import org.scalatest.flatspec.AnyFlatSpec

class StreamTest extends AnyFlatSpec {
  "Stream" should "handle toList" in {
    val stream = Stream(1, 2, 3, 4)
    assert(stream.toList == List(1, 2, 3, 4))
  }

  "Stream" should "handle take N elements" in {
    val stream = Stream(1, 2, 3, 4, 5)
    assert(stream.take(3).toList == List(1, 2, 3))
  }

  "Stream" should "handle drop N elements" in {
    val stream = Stream(1, 2, 3, 4, 5)
    assert(stream.drop(3).toList == List(4, 5))
  }

  "Stream" should "handle takeWhile" in {
    val stream = Stream(1, 2, 3, 4, 5)
    assert(stream.takeWhile(a => a != 3).toList == List(1, 2))
  }

  "Stream" should "handle forAll" in {
    val stream = Stream(1, 2, 3, 4, 5)
    assert(stream.forAll(a => a < 10))
    assert(!stream.forAll(a => a != 3))
  }

//   "Stream" should "handle take N elements via unfold" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     assert(stream.takeViaUnfold(3).toList == List(1, 2, 3))
//   }

//   "Stream" should "handle takeWhile via foldRight" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     assert(stream.takeWhileViaFoldRight(a => a != 3).toList == List(1, 2))
//   }

//   "Stream" should "handle takeWhile via unfold" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     assert(stream.takeWhileViaUnfold(a => a != 3).toList == List(1, 2))
//   }

//   "Stream" should "handle headOption via foldRight" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     val emptyStream = Stream.empty
//     assert(stream.headOption.contains(1))
//     assert(emptyStream.headOption.isEmpty)
//   }

//   "Stream" should "handle map via foldRight" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     val emptyStream = Stream.empty[Int]
//     assert(stream.map(_ + 1).toList == List(2, 3, 4, 5, 6))
//     assert(emptyStream.map(_ + 1).toList == List.empty)
//   }

//   "Stream" should "handle map2 via unfold" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     val emptyStream = Stream.empty[Int]
//     assert(stream.mapViaUnfold(_ + 1).toList == List(2, 3, 4, 5, 6))
//     assert(emptyStream.mapViaUnfold(_ + 1).toList == List.empty)
//   }

//   "Stream" should "handle filter via foldRight" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     val emptyStream = Stream.empty[Int]
//     assert(stream.filter(_ % 2 == 0).toList == List(2, 4))
//     assert(emptyStream.filter(_ % 2 == 0).toList == List.empty)
//   }

//   "Stream" should "handle append via foldRight" in {
//     val stream = Stream(1, 2, 3, 4, 5)
//     val emptyStream = Stream.empty[Int]
//     assert(stream.append(Stream.apply(6)).toList == List(1, 2, 3, 4, 5, 6))
//     assert(emptyStream.append(Stream.apply(1)).toList == List(1))
//   }

//   "Stream" should "handle flatMap via foldRight" in {
//     val stream = Stream(1, 2, 3)
//     val emptyStream = Stream.empty[Int]
//     assert(stream.flatMap(x => Stream.apply(x, x)).toList == List(1, 1, 2, 2, 3, 3))
//     assert(emptyStream.flatMap(x => Stream.apply(x, x)).toList == List.empty)
//   }

//   "Stream" should "handle constant function" in {
//     assert(Stream.constant(3).take(2).toList == List(3, 3))
//   }

//   "Stream" should "handle constant via unfold" in {
//     assert(Stream.constantViaUnfold(3).take(2).toList == List(3, 3))
//   }

//   "Stream" should "handle from function" in {
//     assert(Stream.from(3).take(2).toList == List(3, 4))
//   }

//   "Stream" should "handle from via unfold" in {
//     assert(Stream.fromViaUnfold(3).take(2).toList == List(3, 4))
//   }

//   "Stream" should "handle fibs function" in {
//     assert(Stream.fibs.take(6).toList == List(0, 1, 1, 2, 3, 5))
//   }

//   "Stream" should "handle fibs via unfold" in {
//     assert(Stream.fibsViaUnfold.take(6).toList == List(0, 1, 1, 2, 3, 5))
//   }

//   "Stream" should "handle unfold" in {
//     val infiniteAcual = Stream.unfold(1)(s => Some(s -> s)).take(3).toList
//     val infiniteExpected = List(1, 1, 1)
//     assert(infiniteExpected == infiniteAcual)

//     val finiteActual = Stream.unfold(0)(s => if (s < 5) Some((s, s + 1)) else None).toList
//     val finiteExpected = List(0, 1, 2, 3, 4)
//     assert(finiteExpected == finiteActual)
//   }

//   "Stream" should "handle ones via unfold" in {
//     assert(Stream.onesViaUnfold.take(2).toList == List(1, 1))
//   }

//   "Stream" should "handle zipWith" in {
//     val stream1 = Stream(1, 2, 3)
//     val stream2 = Stream(1, 2, 3, 4)
//     val expected = List(2, 4, 6)
//     assert(stream1.zipWith(stream2)((a, b) => a + b).toList == expected)
//   }

//   "Stream" should "handle zipAll" in {
//     val stream1 = Stream(1, 2, 3)
//     val stream2 = Stream(1, 2, 3, 4)
//     val expected = List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), Some(3)), (None, Some(4)))
//     assert(stream1.zipAll(stream2).toList == expected)
//   }

//   "Stream" should "handle startsWith" in {
//     val stream1 = Stream(1, 2, 3)
//     val stream2 = Stream(2, 3)
//     val stream3 = Stream(1, 2)
//     val stream4 = Stream(1, 2, 3, 4)

//     assert(!stream1.startsWith(stream2))
//     assert(stream1.startsWith(stream3))
//     assert(!stream1.startsWith(stream4))
//   }

//   "Stream" should "handle tails" in {
//     val stream1 = Stream(1, 2, 3)
//     val expected = List(List(1, 2, 3), List(2, 3), List(3), List())
//     assert(stream1.tails.map(s => s.toList).toList == expected)
//   }

//   "Stream" should "handle scanRight" in {
//     val stream1 = Stream(1, 2, 3)
//     val expected = List(6, 5, 3, 0)
//     assert(stream1.scanRight(0)(_ + _).toList == expected)
//   }
}
