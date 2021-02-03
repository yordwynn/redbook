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
}
