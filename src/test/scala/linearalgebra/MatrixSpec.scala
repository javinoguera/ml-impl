package linearalgebra

import linearalgebra.Matrix
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest._

/**
 * Created by javier.noguera on 9/16/16.
 */
class MatrixSpec extends FlatSpec with Matchers{

  it should "initialize matrix" in {
    val matrix = new Matrix(Array(
      Array(1.0,2.0,3.0),
      Array(2.0,3.0,2.0)));
    matrix.rows shouldBe 2
    matrix.cols shouldBe 3
  }

  it should "return a zeros matrix" in {
    val matrix = Matrix.zeros(2,3);

    matrix.rows shouldBe 2;
    matrix.cols shouldBe 3;

    // todo check zeros
  }

  it should "get one column" in {
    val matrix = new Matrix(Array(
      Array(1.0,2.0,3.0),
      Array(2.0,3.0,2.0)))

    var col1 = matrix col 1
    var col2 = matrix col 2

    col1.rows shouldBe 2
    col1.cols shouldBe 1

    col2.rows shouldBe 2
    col2.cols shouldBe 1

//    col1 shouldBe "1.0\t\n2.0\t\n"
//    col2 shouldBe "2.0\t\n3.0\t\n"
  }

  it should "sum all elements" in {
    val matrix = new Matrix(Array(
          Array(1.0,2.0,3.0),
          Array(2.0,3.0,3.0)))

    val sum = Matrix.sum(matrix)

    sum shouldBe 14.0
  }

  it should "negate a matrix" in {
    val matrix = new Matrix(Array(
      Array(1.0,2.0,3.0),
      Array(2.0,3.0,2.0)))

    val neg = -matrix

    neg.rows shouldBe 2
    neg.cols shouldBe 3
  }
//
//    it should "multiple by scalar" in {
//    val matrix = new Matrix(Array(
//      Array(1,2,3),
//      Array(2,3,2)))
//    val matrixTimesScalar = matrix * 2;
//
//    matrixTimesScalar should be new Matrix(new Array(
//      Array(2.0,4.0,6.0),
//      Array(4.0,6.0,4.0)));
//  }
}
