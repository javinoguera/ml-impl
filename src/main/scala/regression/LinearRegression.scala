package regression

import linearalgebra.{Minimizer, BatchGradientDescent, Matrix}
import tranformation.PreProcessable

/**
 * Created by javier.noguera on 10/7/16.
 */
class LinearRegression(minimizer: Minimizer) extends PreProcessable {

  def train(unProcessedx: Matrix, y: Matrix) = {
    val processed = preProcess(unProcessedx)
    val cost = 0.0;
    val x = Matrix.ones(processed.rows,1) ||| processed
    val theta = Matrix.zeros(x.cols,1)

    println("initial theta: \n" + theta);
    println("initial cost: "+costFunction(x, y, theta))

    minimizer.minimize(x,y,theta,costFunction = costFunction, gradientFunction = gradFunction);
  }

  val costFunction = (x:Matrix, y:Matrix, theta:Matrix) => {
    val m = x.rows
    val h0 = x * theta
    val error = h0 - y

    Matrix.sum(error :^ 2) / (2*m)
  }

  val gradFunction = (x:Matrix, y:Matrix, theta:Matrix, i: Int) => {
    require (theta.cols == 1)
    val m = x.rows
    val h0 = x * theta
    val error = h0 - y

    (1.0 / m) * Matrix.sum(error.transpose * (x col i))
  }
}
