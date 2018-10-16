package org.goldenport.record.v3

import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Schema

/*
 * @since   Sep.  2, 2018
 * @version Sep.  4, 2018
 * @author  ASAMI, Tomoharu
 */
trait ITable {
  def toTable: Table
  def toRecordList: List[Record]
}

