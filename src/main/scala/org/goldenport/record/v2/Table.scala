package org.goldenport.record.v2

/*
 * @since   Feb. 11, 2019
 * @version Jun. 23, 2019
 * @author  ASAMI, Tomoharu
 */
case class Table(
  records: Vector[Record],
  schema: Option[Schema]
) extends org.goldenport.table.ITable {
  def print: String = toString
  def display: String = print
  def show: String = display
}
