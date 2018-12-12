package classification

import linearalgebra.{Formula, Minimizer, Matrix}
import tranformation.PreProcessable
import linearalgebra.Formula._

/**
 * Created by javier.noguera on 10/28/16.
 */
class NeuronalNetwork(minimizer: Minimizer, hiddenLayerSize: Int) extends PreProcessable {

  def train(unProcessedx: Matrix, y: Matrix, numLabels: Int, lambda: Double): (Matrix,Double) = {
    val theta1 = randomInitWeights(unProcessedx.cols, hiddenLayerSize);
    val theta2 = randomInitWeights(hiddenLayerSize, numLabels);

    train(unProcessedx, y, numLabels, lambda, theta1, theta2)
  }

  def train(unProcessedx: Matrix, y: Matrix, numLabels: Int, lambda: Double, theta1: Matrix, theta2: Matrix): (Matrix,Double) = {
    val x = preProcess(unProcessedx)

    val cost: Double = costFunction(x, y, theta1, theta2, numLabels, lambda)
    println("initial cost: "+cost)

//    minimizer.minimize(x,y,theta,costFunction = costFunction, gradientFunction = gradFunction)

    (null, cost)
  }

  def randomInitWeights(in:Int, out:Int ): (Matrix) = {
    val epsilonInit = 0.12
    Matrix.zeros(out, in + 1).elementWiseOperation(x => scala.util.Random.nextDouble() *2 * epsilonInit - epsilonInit)
  }

  def sigmoidGradient(z: Matrix): Matrix = {
    sigmoid(z) :* (1.0 - sigmoid(z))
  }

  val costFunction = (x: Matrix, y: Matrix, theta1:Matrix, theta2:Matrix, numLabels: Int, lambda: Double) => {
    var j=0.0;
    val m = x.rows

    var theta1_grad = Matrix.zeros(theta1.rows, theta1.cols)
    var theta2_grad = Matrix.zeros(theta2.rows, theta2.cols)

    for (obs <-1 to m) {
      // forward propagation 1 hidden layer
      println(s"training observation $obs of $m")
      val a1 = Matrix.ones(1,1) +++ x.row(obs).transpose()
      val z2 = theta1 * a1
      val a2 = Matrix.ones(1,1) +++ sigmoid(z2)
      val z3 = theta2 * a2
      val h0 = sigmoid(z3)

      val res = Matrix.zeros(numLabels, 1)
      res.elements(y.row(obs).elements(0)(0).toInt - 1)(0) = 1

      val cost =  ( -res :* log(h0) ) - ((1.0 - res) :* log(1.0-h0))
      j = j + sum(cost)

      // backward propagation
      val delta_3 = h0 - res
      var delta_2 = theta2.transpose() * delta_3
      delta_2 = delta_2.rows(2, delta_2.rows) :* sigmoidGradient(z2)

      theta1_grad = theta1_grad + delta_2 * a1.transpose()
      theta2_grad = theta2_grad + delta_3 * a2.transpose()
    }

    j = j / m

    val theta1reg = theta1.cols(2, theta1.cols)
    val theta2reg = theta2.cols(2, theta2.cols)

    j = j + (sum(theta1reg :^ 2.0) + sum(theta2reg :^ 2.0)) * (lambda/(2*m))

    j
  }
}
