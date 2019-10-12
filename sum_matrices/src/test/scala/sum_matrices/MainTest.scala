package sum_matrices

import org.scalatest.FunSuite

class MainTest extends FunSuite {
  test("Main.test_sum") {
    val d: Array[Array[Int]] = Array(Array(1, 3), Array(1, 0), Array(1, 2))
    val e: Array[Array[Int]] = Array(Array(0, 0), Array(7, 5), Array(2, 1))

    assert(sum_matrices.Main.sum_matrices(List(d, e)).deep ==
      Array(Array(1, 3), Array(8, 5), Array(3, 3)).deep)
  }
}
