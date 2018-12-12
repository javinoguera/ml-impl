package linearalgebra

import scala.math._

/**
 * Created by javier.noguera on 10/28/16.
 */
object Formula {

  val sigmoid = (matrix: Matrix) => {
    matrix.elementWiseOperation((x) => 1 / (1 + exp(-x)))
  }

  val log = (matrix: Matrix) => {
    matrix.elementWiseOperation((x) => scala.math.log(x))
  }

  val sum = (matrix: Matrix) => {
    matrix.elements.flatten.reduce((x,y)=> x+y)
  }
}
