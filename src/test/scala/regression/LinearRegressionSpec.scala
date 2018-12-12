package regression

import input.DataSet
import linearalgebra.{BatchGradientDescent, Matrix}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by javier.noguera on 10/4/16.
 */
class LinearRegressionSpec extends FlatSpec with Matchers{

  it should "perform linear regression with batch gradient descent" in {
    val dataset:Matrix = DataSet.fromCSV("/ex1data1.txt",Array("Population of City in 10,000s","Profit in $10,000s")).split(1.0)
    val iterations = 1500
    val alpha = 0.01

    val y = dataset col 2
    val x = dataset col 1

    val linearRegression = new LinearRegression(new BatchGradientDescent(alpha, iterations))
    val (theta,cost) = linearRegression.train(x,y)

    val epsilon = 1e-3

    theta.elements(0)(0) shouldBe -3.63029143940436 +- epsilon
    theta.elements(1)(0) shouldBe 1.166362350335582 +- epsilon
    cost shouldBe 4.483388256587726 +- epsilon
  }
}
