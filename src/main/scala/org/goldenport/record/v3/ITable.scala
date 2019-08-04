package org.goldenport.record.v3

import org.goldenport.exception.RAISE
import org.goldenport.matrix.IMatrix
import org.goldenport.record.v2.Schema

/*
 * @since   Sep.  2, 2018
 *  version Dec. 27, 2018
 *  version Feb. 11, 2019
 *  version Jun. 23, 2019
 *  version Jul. 26, 2019
 * @version Aug.  3, 2019
 * @author  ASAMI, Tomoharu
 */
trait ITable extends org.goldenport.table.ITable {
  def toTable: Table
  def schema: Schema
  def meta: Table.MetaData // TODO
  def head: Option[Table.Head] // TODO
  def foot: Option[Table.Foot] // TODO
  def data: Table.Data
  def toRecordList: List[Record]
  def toRecordVector: Vector[Record]
}
