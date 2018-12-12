package transformation

import linearalgebra.Matrix
import org.scalatest.{FlatSpec, Matchers}
import tranformation.FeatureNormalization

/**
 * Created by javier.noguera on 10/12/16.
 */
class FeatureNormalizationSpec extends FlatSpec with Matchers {

  it should "perform feature normalization" in {
    var mat = new Matrix(Array(Array(2.0,4.0),Array(4.0,6.0),Array(10.0,100.0)))

    var mean1 = 5.33333
    var std1 = 4.16333

    var mean2 = 36.66667
    var std2 = 54.85739

    val normalized = new FeatureNormalization {}.preProcess(mat)

    val epsilon = 1e-3

    normalized.elements(0)(0) shouldBe (2-mean1)/std1 +- epsilon
    normalized.elements(1)(0) shouldBe (4-mean1)/std1 +- epsilon
    normalized.elements(2)(0) shouldBe (10-mean1)/std1 +- epsilon

    normalized.elements(0)(1) shouldBe (4-mean2)/std2 +- epsilon
    normalized.elements(1)(1) shouldBe (6-mean2)/std2 +- epsilon
    normalized.elements(2)(1) shouldBe (100-mean2)/std2 +- epsilon
  }

}
