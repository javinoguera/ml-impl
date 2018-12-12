package input

import linearalgebra.Matrix

import scala.language.implicitConversions

import scala.collection.mutable.ListBuffer

/**
 * Created by javier.noguera on 9/20/16.
 */
class DataSet(val values:Array[Array[String]], val colLabels: Array[String]) {

  def split(sizes: Double*) = {
    // Check size goes to 100
    // split
    this
  }
}

object DataSet {

  def fromCSV(filename: String, colNames: Array[String]) =  {
    val bufferedSource = io.Source.fromInputStream(getClass.getResourceAsStream(filename))
    var data = Array[Array[String]]()
    for (line <- bufferedSource.getLines) {
      val row = line.split(",").map(_.trim)
      data = data :+ row
    }

    bufferedSource.close()

    new DataSet(data, colNames)
  }

  implicit def datasetToMatrix(dataset: DataSet): Matrix = {
    new Matrix(dataset.values.map((x) => x.map((y) => y.toDouble)))
  }
}

