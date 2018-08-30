package org.goldenport.record.v3

import org.goldenport.exception.RAISE
import org.goldenport.record.v2.Schema

/*
 * @since   Aug. 24, 2018
 * @version Aug. 24, 2018
 * @author  ASAMI, Tomoharu
 */
case class Table(
  records: Vector[Record],
  meta: Table.MetaData = Table.MetaData.empty
) {
}

object Table {
  case class MetaData(
    schema: Option[Schema]
  )
  object MetaData {
    val empty = MetaData(None)
  }
}
