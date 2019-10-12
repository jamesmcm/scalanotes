package sum_matrices

object Main extends App {
  def sum_vector(x: Array[Int], y: Array[Int]): Array[Int] = {
    (x zip y).map((a: (Int, Int)) => a._1 + a._2)

  }

  def sum_matrix(x: Array[Array[Int]], y: Array[Array[Int]]): Array[Array[Int]] = {
    (x zip y).map((a: (Array[Int],  Array[Int])) => sum_vector(a._1, a._2))

  }


  def sum_matrices(x: List[Array[Array[Int]]]): Array[Array[Int]] = {
    x.foldLeft(new Array[Array[Int]](x.head.length).map(_ => Array.fill(x.head.head.length)(0)))(sum_matrix)
  }

}

