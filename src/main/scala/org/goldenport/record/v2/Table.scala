package org.goldenport.record.v2

/*
 * @since   Feb. 11, 2019
 * @version Feb. 11, 2019
 * @author  ASAMI, Tomoharu
 */
case class Table(
  records: Vector[Record],
  schema: Option[Schema]
) extends org.goldenport.table.ITable {
}
