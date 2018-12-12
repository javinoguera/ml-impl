package tranformation

import linearalgebra.Matrix

/**
 * Created by javier.noguera on 10/11/16.
 */
trait FeatureNormalization extends PreProcessable {

  abstract override def preProcess (matrix: Matrix): Matrix = {
    val count = matrix.rows

    var normalizedMatrix = matrix

    for (col <-1 to matrix.cols) {
      val colElements = matrix.col(col).elements.flatten
      val mu = colElements.sum / count
      val deviations = colElements.map((x) => Math.pow(x - mu,2))
      val sigma = Math.sqrt((deviations.sum) / (count-1))

      normalizedMatrix = normalizedMatrix.col(col, (x) => (x - mu)/sigma)
    }

    super.preProcess(normalizedMatrix)
  }
}
