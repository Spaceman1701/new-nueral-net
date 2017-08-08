package ethan.network.math

import java.lang.IndexOutOfBoundsException

import org.scalatest.{FlatSpec, Matchers}


class TestMatrix extends FlatSpec with Matchers{
  "A Matrix" should "be built constructed as an identity Matrix" in {
    val matrix = Matrix.identity(3, 3)
    matrix.get(0, 0) should be (1)
    matrix.get(1, 1) should be (1)
    matrix.get(2, 2) should be (1)
    matrix.get(0, 2) should be (0)
  }

  it should "throw an IndexOutOfBounds exception if an index is out of bounds" in {
    val matrix = Matrix.identity(5, 1)
    a [IndexOutOfBoundsException] should be thrownBy {
      matrix.get(5, 0)
    }
  }
}
