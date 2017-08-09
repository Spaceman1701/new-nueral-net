package ethan.network.math

class Matrix(mat: Array[Array[Float]]) {
  def get(row: Int, col: Int): Float = mat(row)(col)

  def apply(row: Int, col: Int): Float = get(row, col)

  def dim: (Int, Int) = (mat.length, mat(0).length)

  def getRow(i: Int): Array[Float] = {
    mat(i)
  }

  def getCol(i: Int): Array[Float] = {
    val col = for (row <- mat) yield row(i)
    col
  }

  def * (that: Matrix): Matrix = {
    Matrix.mul(this, that)
  }

  def entryProduct (that: Matrix): Matrix = {
    Matrix.create(this.dim, (row, col) => this(row, col) * that(row, col))
  }

  override def hashCode(): Int = {
    var hash = 7
    for (r <- mat.indices) {
      for (c <- mat(0).indices) {
        hash += r + c
        hash = hash << 7
      }
    }
    hash
  }

  override def equals(that: scala.Any): Boolean = {
    that match {
      case that: Matrix => equalsMat(that)
      case _ => false
    }
  }

  def equalsMat(that: Matrix): Boolean = {
    if (dim != that.dim) {
      return false
    }
    var result = true
    for (r <- mat.indices) {
      for (c <- mat(0).indices) {
        result &= this(r, c) == that(r, c)
      }
    }
    result
  }
}

object Matrix {
  def identity(rows: Int, cols: Int): Matrix = {
    create((rows, cols), (row, col) => if (row == col) 1.0F else 0.0F)
  }


  def create(dim: (Int, Int), fill: (Int, Int) => Float) = {
    val matrix = Array.tabulate(dim._1, dim._2)(fill)
    new Matrix(mat = matrix)
  }

  private def mul(left: Matrix, right: Matrix): Matrix = {
    val dim = (left.dim._1, right.dim._2)
    create(dim, (r, c) => dot(left.getRow(r), right.getCol(c)))
  }

  private def dot(left: Array[Float], right: Array[Float]): Float = {
    var result: Float = 0.0F
    for (i <- left.indices) {
      result += left(i) * right(i)
    }
    result
  }
}
