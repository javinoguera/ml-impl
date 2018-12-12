package linearalgebra

import com.github.lbfgs4j.LbfgsMinimizer

/**
 * Created by javier.noguera on 10/26/16.
 */
class LBFGS extends Minimizer {
  override def minimize(x: Matrix, y: Matrix, theta: Matrix, costFunction: (Matrix, Matrix, Matrix) => Double, gradientFunction: (Matrix, Matrix, Matrix, Int) => Double): (Matrix, Double) = {
    val f = new com.github.lbfgs4j.liblbfgs.Function {
      override def getDimension: Int = theta.rows

      override def gradientAt(theta: Array[Double]): Array[Double] = (for (i <-1 to theta.length) yield {gradientFunction(x,y,new Matrix(theta),i)}).toArray

      override def valueAt(theta: Array[Double]): Double = costFunction(x, y, new Matrix(theta))
    }

    val minimizer = new LbfgsMinimizer()

    val thetaOpt: Array[Double] = minimizer.minimize(f)

    val cost = f.valueAt(thetaOpt)

    (new Matrix(thetaOpt), cost)
  }
}
