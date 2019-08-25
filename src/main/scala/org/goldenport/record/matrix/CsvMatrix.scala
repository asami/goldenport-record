package org.goldenport.record.matrix

import java.net.URI
import org.goldenport.RAISE
import org.goldenport.matrix._
import org.goldenport.record.v2.bag.CsvBag

/*
 * @since   Feb. 10, 2019
 *  version Feb. 11, 2019
 *  version Jun. 23, 2019
 *  version Jul. 16, 2019
 * @version Aug. 26, 2019
 * @author  ASAMI, Tomoharu
 */
case class CsvMatrix(bag: CsvBag) extends IMatrix[Double] {
  lazy val matrix = bag.toMatrixDouble
  // lazy val breeze: Matrix[Double] = {
  //   val m = bag.matrixDouble
  //   DenseMatrix.tabulate(m.height, m.width)((i, j) => m(i, j))
  // }

  def apply(x: Int, y: Int): Double = matrix.apply(x, y)
  def width: Int = matrix.width
  def height: Int = matrix.height
  def rowIterator: Iterator[Vector[Double]] = matrix.rowIterator
  def columnIterator: Iterator[Vector[Double]] = matrix.columnIterator

  def appendRow(ps: Seq[Double]): CsvMatrix = RAISE.unsupportedOperationFault
  def appendRows(ps: IMatrix[Double]): CsvMatrix = RAISE.notImplementedYetDefect

  def transpose: IMatrix[Double] = RAISE.notImplementedYetDefect
}

object CsvMatrix {
  def load(uri: URI): CsvMatrix = CsvMatrix(CsvBag.load(uri, CsvBag.Strategy.matrixAuto))
  def loadUri(uri: String): CsvMatrix = CsvMatrix(CsvBag.loadUri(uri, CsvBag.Strategy.matrixAuto))
}
