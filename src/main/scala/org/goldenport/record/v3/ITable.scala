package org.goldenport.record.v3

import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Schema

/*
 * @since   Sep.  2, 2018
 *  version Dec. 27, 2018
 * @version Feb. 11, 2019
 * @author  ASAMI, Tomoharu
 */
trait ITable extends org.goldenport.table.ITable {
  def toTable: Table
  def toRecordList: List[Record]
  def toRecordVector: Vector[Record]
}

