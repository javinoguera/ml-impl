package linearalgebra

import linearalgebra.Matrix

/**
 * Created by javier.noguera on 10/19/16.
 */
trait Minimizer {

  def minimize(x: Matrix, y: Matrix, theta: Matrix,
               costFunction: (Matrix, Matrix, Matrix) => (Double),
               gradientFunction: (Matrix, Matrix, Matrix, Int) => (Double)): (Matrix, Double)

}
