package org.goldenport.record.v2

/*
 * @since   Feb. 11, 2019
 *  version Jun. 23, 2019
 * @version Jul. 28, 2019
 * @author  ASAMI, Tomoharu
 */
case class Table(
  records: Vector[Record],
  schema: Option[Schema]
) extends org.goldenport.table.ITable {
  def width = schema.map(_.columns.length).getOrElse(_width_by_records)
  def height = records.length
  def print: String = toString
  def display: String = print
  def show: String = display

  private lazy val _width_by_records = {
    case class Z(names: Set[Symbol] = Set.empty) {
      def r = names.size

      def +(rhs: Record) = Z(names ++ rhs.fields.map(_.key))
    }
    records./:(Z())(_+_).r
  }
}
