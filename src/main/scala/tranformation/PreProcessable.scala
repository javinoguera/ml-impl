package tranformation

import linearalgebra.Matrix

/**
 * Created by javier.noguera on 10/11/16.
 */
trait PreProcessable {

  def preProcess (matrix: Matrix): Matrix = {matrix}
}
