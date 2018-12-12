package classification

import input.DataSet
import linearalgebra.{LBFGS, BatchGradientDescent, Matrix}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by javier.noguera on 10/4/16.
 */
class LogisticRegressionSpec extends FlatSpec with Matchers{

  it should "perform logistic regression with LBFGS" in {
    val dataset:Matrix = DataSet.fromCSV("/ex2data1.txt",Array("Exam 1 score","Exam 2 score")).split(1.0)
    val iterations = 300000
    val alpha = 0.0011

    val y = dataset col 3
    val x = (dataset col 1) ||| (dataset col 2)

    val logisticRegression = new LogisticRegression(new LBFGS())
    val (theta,cost) = logisticRegression.train(x,y)

    val epsilon = 1e-3

    cost shouldBe 0.203 +- epsilon
  }
}
