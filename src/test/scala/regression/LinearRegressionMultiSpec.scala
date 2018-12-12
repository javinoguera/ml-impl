package regression

import input.DataSet
import linearalgebra.{BatchGradientDescent, Matrix}
import org.scalatest.{FlatSpec, Matchers}
import tranformation.FeatureNormalization

/**
 * Created by javier.noguera on 10/4/16.
 */
class LinearRegressionMultiSpec extends FlatSpec with Matchers{


  it should "perform linear regression with feature regularization with multiple variables" in {
    val dataset:Matrix = DataSet.fromCSV("/ex1data2.txt", Array("")).split(1.0)
    val iterations = 400
    val alpha = 0.01

    val y = dataset col 3
    val x = (dataset col 1) ||| (dataset col 2)

    val gradientDescent = new BatchGradientDescent(alpha, iterations)

    val linearRegression = new LinearRegression(gradientDescent) with FeatureNormalization

    val (theta,cost) = linearRegression.train(x,y)

    val epsilon = 1e-3

    val pred = theta.transpose * new Matrix(Array(1, 1650.0, 3.0))

    theta.elements(0)(0) shouldBe 334302.063993 +- epsilon
    theta.elements(1)(0) shouldBe 100087.116006 +- epsilon
    theta.elements(2)(0) shouldBe 3673.548451 +- epsilon
  }
}
