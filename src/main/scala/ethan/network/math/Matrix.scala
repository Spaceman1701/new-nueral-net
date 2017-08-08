package ethan.network.math

class Matrix(mat: Array[Array[Int]]) {
  def get(col: Int, row: Int) = mat(col)(row)
}

object Matrix {
  def identity(rows: Int, cols: Int): Matrix = {
    val matrix = Array.tabulate(cols, rows)((col, row) => if (row == col) 1 else 0)
    new Matrix(matrix)
  }
}
