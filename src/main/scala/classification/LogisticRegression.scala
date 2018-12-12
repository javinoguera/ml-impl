package classification

import linearalgebra.{Formula, Minimizer, BatchGradientDescent, Matrix}
import tranformation.PreProcessable

import scala.math._

/**
 * Created by javier.noguera on 10/13/16.
 */
class LogisticRegression(minimizer: Minimizer) extends PreProcessable {

  def train(unProcessedx: Matrix, y: Matrix) = {
    val processed = preProcess(unProcessedx)
    val x = Matrix.ones(processed.rows,1) ||| processed
    val theta = Matrix.zeros(x.cols,1)

    println("initial theta: \n" + theta);
    println("initial cost: "+costFunction(x, y, theta))

    minimizer.minimize(x,y,theta,costFunction = costFunction, gradientFunction = gradFunction);
  }

  val costFunction = (x: Matrix, y: Matrix, theta:Matrix) => {
    val h0 = Formula.sigmoid(x*theta)
    val m = x.rows

    Matrix.sum(((-y) :* Matrix.log(h0)) - ((1.0-y) :* Matrix.log(1.0-h0)))/m
  }

  val gradFunction = (x:Matrix, y:Matrix, theta:Matrix, i: Int) => {
    require (theta.cols == 1)
    val m = x.rows
    val h0 = Formula.sigmoid(x * theta)
    val error = h0 - y

    (1.0 / m) * Matrix.sum(error.transpose * (x col i))
  }
}
