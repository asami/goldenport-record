package org.goldenport.record.v2

import org.goldenport.matrix._
import org.goldenport.record.v3.{IRecord, FieldValue}

/*
 * @since   Feb. 11, 2019
 *  version Jun. 23, 2019
 *  version Jul. 28, 2019
 *  version Aug.  3, 2019
 * @version Oct. 16, 2019
 * @author  ASAMI, Tomoharu
 */
case class Table(
  schema: Schema,
  records: Vector[Record]
) extends org.goldenport.table.ITable {
  def width = schema.columns.length
  def height = records.length
  def matrix: IMatrix[Any] = VectorRowColumnMatrix(records.map(_.fields.map(_make_value).toVector))
  def print: String = toString
  def display: String = print
  def show: String = display
  def embed: String = display

  private def _make_value(p: Field): Any = schema.getColumn(p.key).
    map(c => p.toFieldValue.getValue.map(c.datatype.toInstance).getOrElse(Table.Empty)).
    getOrElse(p.toFieldValue.getValue.getOrElse(Table.Empty))

  // private lazy val _width_by_records = {
  //   case class Z(names: Set[Symbol] = Set.empty) {
  //     def r = names.size

  //     def +(rhs: Record) = Z(names ++ rhs.fields.map(_.key))
  //   }
  //   records./:(Z())(_+_).r
  // }
}

object Table {
  val Empty = org.goldenport.record.v3.Table.Empty

  def create(schema: Option[Schema], ps: Seq[Record]): Table = schema.
    map(Table(_, ps.toVector)).
    getOrElse(Table(IRecord.makeSchema(ps.map(RecordRecord(_))), ps.toVector))
}
