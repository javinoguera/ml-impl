package classification

import input.DataSet
import linearalgebra.{LBFGS, Matrix}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by javier.noguera on 10/4/16.
 */
class NeuronalNetworkSpec extends FlatSpec with Matchers{

  it should "perform neuronal network with given theta" in {
    val x:Matrix = DataSet.fromCSV("/ex4X.csv",Array("")).split(1.0)
    val y:Matrix = DataSet.fromCSV("/ex4y.csv",Array("")).split(1.0)
    val theta1:Matrix = DataSet.fromCSV("/ex4Theta1.csv",Array("")).split(1.0)
    val theta2:Matrix = DataSet.fromCSV("/ex4Theta2.csv",Array("")).split(1.0)

    val neuronalNetwork = new NeuronalNetwork(new LBFGS(), hiddenLayerSize = 25)
    val (theta,cost) = neuronalNetwork.train(x,y,10,1.0, theta1,theta2)

    val epsilon = 1e-2

    cost shouldBe 0.383770 +- epsilon
  }
}
