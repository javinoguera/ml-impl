package input

import linearalgebra.Matrix
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by javier.noguera on 9/20/16.
 */
class DataSetSpec extends FlatSpec with Matchers{

  it should "load from csv into a matrix" in {
    val x:Matrix= DataSet.fromCSV("/ex1data1.txt",Array("Population of City in 10,000s","Profit in $10,000s")).split(1.0)

    x.cols shouldBe 2
    x.rows shouldBe 97
  }
}
