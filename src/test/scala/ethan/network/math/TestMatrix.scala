package ethan.network.math

import java.lang.IndexOutOfBoundsException

import org.scalatest.{FlatSpec, Matchers}


class TestMatrix extends FlatSpec with Matchers{
  "A Matrix" should "be built constructed as an identity Matrix with correct dimension" in {
    val matrix = Matrix.identity(3, 3)
    matrix.dim should be (3, 3)
    matrix(0, 0) should be (1)
    matrix(1, 1) should be (1)
    matrix(2, 2) should be (1)
    matrix(0, 2) should be (0)
  }

  it should "throw an IndexOutOfBounds exception if an index is out of bounds" in {
    val matrix = Matrix.identity(5, 1)
    a [IndexOutOfBoundsException] should be thrownBy {
      matrix.get(5, 0)
    }
  }

  "Multiplying two identity matrices " should "produce an identity matrix" in {
    val mat1 = Matrix.identity(3, 3)
    val mat2 = Matrix.identity(3, 3)
    val res = mat1 * mat2

    res(0, 0) should be (1)
    res(1, 1) should be (1)
    res(2, 2) should be (1)
    res(0, 1) should be (0)
  }

  "Two identical matrices " should "be equal" in {
    val mat1 = Matrix.identity(4, 2)
    val mat2 = Matrix.identity(4, 2)

    mat1.equals(mat2) should be (true)
    mat2.equals(mat1) should be (true)
  }

  "Two different matrices" should "be different" in {
    val matrix1Array: Array[Array[Float]] = Array.tabulate(2, 2)((x, y) => 0.0F)
    val matrix2Array: Array[Array[Float]] = Array.tabulate(2, 2)((x, y) => 1.0F)

    val mat1 = new Matrix(matrix1Array)
    val mat2 = new Matrix(matrix2Array)

    mat1.equals(mat2) should be (false)
    mat2.equals(mat1) should be (false)
  }

  "Matrices of different sizes" should "not be equal" in {
    val mat1 = Matrix.identity(3, 2)
    val mat2 = Matrix.identity(4, 1)

    mat1.equals(mat2) should be (false)
  }
}
