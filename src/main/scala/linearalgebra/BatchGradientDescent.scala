package linearalgebra

/**
 * Created by javier.noguera on 10/19/16.
 */
class BatchGradientDescent(alpha: Double, iterations: Int) extends Minimizer {

  override def minimize(x: Matrix, y: Matrix, initialTheta: Matrix,
                        costFunction: (Matrix, Matrix, Matrix) => (Double),
                        gradientFunction: (Matrix, Matrix, Matrix, Int) => (Double)): (Matrix, Double) = {
    var theta = initialTheta
    var cost = 0.0d
    for (i <- 1 to iterations) {
      cost = costFunction(x, y, theta)
      theta = computeTheta(x, y, theta, alpha, gradientFunction)
      println("Current theta:\n " + theta)
      println("Current cost: " + cost)
    }

    (theta, cost)
  }

  def computeTheta = (x: Matrix, y: Matrix, theta: Matrix, alpha: Double, gradientFunction: (Matrix, Matrix, Matrix, Int) => (Double)) => {
    require(theta.cols == 1)

    var newTheta = new Matrix(theta.elements);

    for (i <- 1 to theta.rows) {
      newTheta = newTheta.row(i, (theta row i) - alpha * gradientFunction(x,y,theta,i))
    }

    newTheta;
  }
}
