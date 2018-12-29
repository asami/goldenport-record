package org.goldenport.record.v3

import play.api.libs.json._
import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Schema

/*
 * @since   Aug. 24, 2018
 *  version Sep.  4, 2018
 * @version Dec. 27, 2018
 * @author  ASAMI, Tomoharu
 */
case class Table(
  records: Vector[Record],
  meta: Table.MetaData = Table.MetaData.empty
) extends ITable {
  def toTable = this
  def toRecordList: List[Record] = records.toList
  def toRecordVector: Vector[Record] = records
}

object Table {
  val empty = Table(Vector.empty)

  case class MetaData(
    schema: Option[Schema]
  )
  object MetaData {
    val empty = MetaData(None)
  }

  def create(a: JsArray): Table = RAISE.notImplementedYetDefect
}
