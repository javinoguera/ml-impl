package linearalgebra

import scala.Array._

/**
  * Created by javier.noguera on 9/16/16.
  */
class Matrix(val elements: Array[Array[Double]]) {

  val rows = {elements.length}
  val cols = {elements(0).length}

  require(elements.length>0);

  def this (vector: Array[Double]) = {
    this(vector.map( x => Array(x)))
  }

  def col(colNumber: Int) = {
    require(colNumber <= cols && colNumber>=1)

    new Matrix(elements.map(x => x.apply(colNumber - 1)))
  }

  def col(colNumber: Int, f: (Double) => Double): Matrix = {
    new Matrix(this.elements.map(_.zipWithIndex.map{case(e,i)=>if (i==colNumber-1) f(e) else e}))
  }

  def cols(colStart: Int, colEnd:Int): Matrix = {
    require(colEnd <= cols)
    require(colStart >= 1)
    require(colEnd <= colEnd)
    require(colEnd > colStart)

    new Matrix(elements.map(x => x.slice(colStart-1, colEnd-1)))
  }

  def row(rowNumber: Int): Matrix = {
    require(rowNumber <= rows)

    new Matrix(elements(rowNumber - 1)).transpose()
  }

  def rows(rowStart: Int, rowEnd: Int): Matrix = {
    require(rowEnd <= rows)
    require(rowEnd > rowStart)
    require(rowStart > 1)

    val filteredRows = for (rowNumber <- rowStart-1 to rowEnd-1) yield {elements(rowNumber)}
    new Matrix(filteredRows.toArray)
  }

  def row(rowNumber: Int, matrix: Matrix) = {
    require (rowNumber>0 && rowNumber<=this.rows && matrix.rows==1 && matrix.cols == this.cols)
    new Matrix(this.elements.zipWithIndex.map{case (e,i) => if (i==rowNumber-1) matrix.elements(0) else e})
  }

  def |||(col: Matrix) = {
    new Matrix(elements.zip(col.elements).map((x) =>  x._1 ++ x._2))
  }

  def +++(row: Matrix) = {
    new Matrix(elements ++ row.elements)
  }

  def :^(scalar: Double) = {
    elementWiseOperation((x) => scala.math.pow(x,scalar))
  }

  def :*(scalar: Double) = {
    elementWiseOperation((x) => x*scalar)
  }

  def :*(that: Matrix): Matrix = {
    require(this.cols==that.cols)
    require(this.rows==that.rows)

    elementWiseOperation(that, (x,y) => x*y)
  }

  def *(that: Matrix) = {
    require(this.cols==that.rows)
    new Matrix (for (row <- this.elements)
      yield for (col <- that.elements.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_))
  }

  def +(that: Matrix) = {
    elementWiseOperation(that, (x,y) => x+y)
  }

  def -(that: Matrix) = {
    elementWiseOperation(that, (x,y) => x-y)
  }

  def unary_- = {
    elementWiseOperation((x) => -x)
  }

  def -(scalar: Double) = {
    elementWiseOperation((x) => x-scalar)
  }

  def transpose() = {
    new Matrix(this.elements.transpose)
  }

  def elementWiseOperation(that: Matrix, operation: (Double, Double) => Double): Matrix = {
    require(this.cols==that.cols && this.rows==that.rows)
    new Matrix(this.elements
      .zip(that.elements)
      .map{case(x,y) => x.zip(y).map{case(w,z) => (operation(w,z))}})
  }

  def elementWiseOperation(operation: (Double) => Double): Matrix = {
    new Matrix(this.elements
      .map((x) => x.map((y) => operation(y))))
  }

  override def toString: String = {
    "Matrix of "+this.rows+" x "+this.cols
  }

   def prettyPrint: String = {
     var str = ""
     for (row <- 0 to rows-1) {
       for (col <-0 to cols-1) {
         str = str + elements(row)(col)+"\t";
       }
       str=str+"\n"
     }
     str
   }
 }

object Matrix  {
  def log(matrix: Matrix): Matrix = {
    matrix.elementWiseOperation((x) => scala.math.log(x))
  }

  def sum(matrix: Matrix): Double = {
    matrix.elements.flatten.reduce((x,y)=> x+y)
  }

  def ones(rows: Int, cols: Int) = {
    new Matrix(Array.fill(rows, cols)(1.0))
  }

  def zeros(rows: Int, cols: Int) = {
    new Matrix(Array.fill(rows, cols)(0.0))
  }

  implicit def doubleToDoubleScalar(scalar: Double): DoubleScalar = {
    new DoubleScalar(scalar)
  }
}

class DoubleScalar(value: Double) {

  def +(matrix: Matrix): Matrix = {
    matrix.elementWiseOperation((x) => value+x)
  }

  def -(matrix: Matrix): Matrix = {
    matrix.elementWiseOperation((x) => value-x)
  }


}

